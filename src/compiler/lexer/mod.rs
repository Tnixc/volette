use std::collections::VecDeque;

use crate::compiler::tokens::TokenKind;

use super::tokens::{Span, Token};
pub mod error;
use error::LexError;
mod bool;
use string_interner::{StringInterner, backend::BucketBackend, symbol::SymbolUsize};
mod keywords;
mod numbers;
mod punctuation;
mod type_literals;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum LexerState {
    Number(bool, numbers::NumberBase),
    String,
    Normal,
    Comment,
    MultilineComment,
    End,
}

#[derive(Debug, Clone)]
pub struct Cursor {
    pub file: SymbolUsize,
    pub line: usize,
    pub col: usize,
}

#[derive(Debug)]
pub struct Lexer<'a> {
    pub tokens: Vec<Token>,
    pub state: LexerState,
    pub errors: Vec<LexError>,
    pub cursor: Cursor,
    pub interner: &'a mut StringInterner<BucketBackend<SymbolUsize>>,
    pub newline: bool,
    pub current_chars: VecDeque<((usize, usize), char)>,
}

impl<'a> Lexer<'a> {
    pub fn new(interner: &'a mut StringInterner<BucketBackend<SymbolUsize>>, path: SymbolUsize) -> Self {
        Lexer {
            tokens: vec![Token::new(TokenKind::Start, Span::new(path, 0, 0, 0, 0))],
            state: LexerState::Normal,
            errors: Vec::new(),
            cursor: Cursor {
                file: path,
                line: 1,
                col: 0,
            },
            current_chars: VecDeque::new(),
            interner,
            newline: false,
        }
    }

    pub fn tokenize(&mut self, mut chars: Vec<char>) {
        chars.extend_from_slice(&['\0'; 8]);

        let mut iter = chars.into_iter();
        let mut curr = iter.next().unwrap_or('\0');
        let mut next = iter.next().unwrap_or('\0');

        for ch in iter {
            self.next(curr, next);
            curr = next;
            next = ch;
        }
    }

    pub fn next(&mut self, c: char, next: char) {
        if self.newline {
            self.cursor.line += 1;
            self.cursor.col = 0;
            self.newline = false;
        }

        if c != '\0' {
            self.cursor.col += 1;
        }

        while let Some(&((_, _), first_char)) = self.current_chars.front() {
            if first_char.is_whitespace() {
                self.current_chars.pop_front();
            } else {
                break;
            }
        }

        if !c.is_allowed_char() {
            self.errors.push(LexError::InvalidCharacter {
                character: c,
                span: self
                    .create_span(self.cursor.line, self.cursor.col, self.cursor.line, self.cursor.col)
                    .to_display(&self.interner),
            });
            return;
        }

        match self.state {
            LexerState::Normal => {
                if c.is_numeric()
                    && (self.current_chars.is_empty() || (self.current_chars.len() == 1 && !self.current_chars[0].1.is_alphanumeric()))
                {
                    // if current_chars has a single non-alphanumeric character (like punctuation),
                    // process it first, then start number parsing
                    if !self.current_chars.is_empty() {
                        self.check_punctuation();
                    }
                    self.state = LexerState::Number(c == '.', numbers::NumberBase::Decimal);
                    self.current_chars.push_back(((self.cursor.line, self.cursor.col), c));
                } else if c == '/' && next == '*' {
                    self.state = LexerState::MultilineComment;
                } else if c == '/' && next == '/' {
                    self.state = LexerState::Comment;
                } else {
                    self.current_chars.push_back(((self.cursor.line, self.cursor.col), c));
                    if !self.check_punctuation()
                        && !self.check_type_literals()
                        && !self.check_keywords()
                        && !self.check_bool()
                        && !self.check_nil()
                        && !c.is_valid_ident_char()
                        && !self.current_chars.is_empty()
                        && self.current_chars.len() > 1
                    {
                        let current_len = self.current_chars.len();
                        let ident_range = 0..current_len.saturating_sub(1);

                        let ident_chars: Vec<_> = ident_range.clone().map(|i| self.current_chars[i]).collect();

                        if ident_chars.iter().all(|&(_, ch)| ch.is_valid_ident_char() || ch.is_whitespace()) {
                            let identifier: String = ident_chars.iter().map(|(_, ch)| *ch).filter(|ch| !ch.is_whitespace()).collect();

                            if !identifier.is_empty() {
                                let start_pos = ident_chars[0].0;
                                let end_pos = ident_chars[ident_chars.len() - 1].0;
                                let span = self.create_span(start_pos.0, start_pos.1, end_pos.0, end_pos.1);
                                self.tokens
                                    .push(Token::new(TokenKind::Identifier(self.interner.get_or_intern(&identifier)), span));
                            }

                            let current_char = self.current_chars.pop_back();
                            self.current_chars.clear();
                            if let Some(ch) = current_char {
                                self.current_chars.push_back(ch);
                            }
                        }
                    }
                }
                if c == '\n' {
                    self.newline = true;
                }
            }
            LexerState::Number(float, base) => {
                if !self.lex_number(c, float, base) {
                    self.cursor.col -= 1;
                    self.next(c, next);
                }
            }
            LexerState::Comment => {
                if c == '\n' {
                    self.state = LexerState::Normal;
                    self.cursor.line += 1;
                    self.cursor.col = 0;
                }
            }
            LexerState::MultilineComment => {
                if c == '*' {
                    self.current_chars.clear();
                    self.current_chars.push_back(((self.cursor.line, self.cursor.col), c));
                } else if c == '/' && self.current_chars.front().is_some_and(|(_, ch)| *ch == '*') {
                    self.current_chars.clear();
                    self.state = LexerState::Normal;
                } else if c == '\n' {
                    self.cursor.line += 1;
                    self.cursor.col = 0;
                }
            }
            LexerState::String => {
                // TODO: Implement string literal lexing
                self.state = LexerState::Normal;
            }
            LexerState::End => {
                return;
            }
        }
    }

    fn create_span(&self, start_line: usize, start_col: usize, end_line: usize, end_col: usize) -> Span {
        Span::new(self.cursor.file, start_line, start_col, end_line, end_col)
    }
}

pub trait LexedChar {
    fn is_valid_ident_char(&self) -> bool;
    fn is_allowed_char(&self) -> bool;
}

impl LexedChar for char {
    fn is_valid_ident_char(&self) -> bool {
        self.is_alphanumeric() || *self == '_' || *self == '$' || *self == '#'
    }

    fn is_allowed_char(&self) -> bool {
        let disallowed_chars = ['^', '`', '~', '\'', '"', '?', '\\', '@'];
        !disallowed_chars.contains(self) && self.is_ascii()
    }
}

impl<'a> Lexer<'a> {
    pub fn format_tokens(&self) -> Vec<(String, usize, (usize, usize))> {
        self.tokens
            .iter()
            .map(|t| {
                (
                    if let TokenKind::Identifier(identifier) = t.kind {
                        format!("Identifier({})", self.interner.resolve(identifier).unwrap_or("<unknown>"))
                    } else {
                        format!("{:?}", t.kind)
                    },
                    t.span.start.0,
                    (t.span.start.1, t.span.end.1),
                )
            })
            .collect::<Vec<_>>()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use string_interner::{StringInterner, backend::BucketBackend, symbol::SymbolUsize};
    type Interner = StringInterner<BucketBackend<SymbolUsize>>;

    #[test]
    fn test_lex_identifiers_with_keywords() {
        let mut interner = Interner::new();
        let f = interner.get_or_intern("");
        let contents = "let__ use some_ident normal";
        let mut lexer = Lexer::new(&mut interner, f);

        let chars: Vec<char> = contents.chars().chain(std::iter::once('\0')).collect();
        lexer.tokenize(chars);

        let expected_tokens = vec![
            ("Identifier(let__)".to_string(), 1, (1, 5)),
            ("Keyword(Use)".to_string(), 1, (7, 9)),
            ("Identifier(some_ident)".to_string(), 1, (11, 20)),
            ("Identifier(normal)".to_string(), 1, (22, 27)),
        ];

        let tokens = lexer.format_tokens();
        assert_eq!(tokens.len() - 1, expected_tokens.len());

        for (i, token) in tokens.iter().skip(1).enumerate() {
            assert_eq!(token, &expected_tokens[i]);
        }
    }
}
