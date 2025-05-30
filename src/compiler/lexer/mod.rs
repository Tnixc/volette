use crate::compiler::tokens::TokenKind;

use super::tokens::{Span, Token};
mod error;
use error::LexError;
use string_interner::{backend::BucketBackend, symbol::SymbolUsize, StringInterner};
mod keywords;
mod numbers;
mod punctuation;
mod type_literals;

#[derive(Debug, Clone, Copy, PartialEq)]
#[allow(dead_code)]
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
pub struct Lexer {
    pub tokens: Vec<Token>,
    pub state: LexerState,
    pub errors: Vec<LexError>,
    pub cursor: Cursor,
    pub interner: StringInterner<BucketBackend<SymbolUsize>>,
    pub newline: bool,
    pub current_chars: Vec<((usize, usize), char)>, // (line, col), char
}

impl Lexer {
    pub fn new(str: &str, interner: StringInterner<BucketBackend<SymbolUsize>>, path: SymbolUsize) -> Self {
        Lexer {
            tokens: Vec::with_capacity(str.len() / 5),
            state: LexerState::Normal,
            errors: Vec::new(),
            cursor: Cursor {
                file: path,
                line: 1,
                col: 0,
            },
            current_chars: Vec::with_capacity(32),
            interner,
            newline: false,
        }
    }

    pub fn tokenize(&mut self, mut chars: Vec<char>) {
        chars.extend_from_slice(&['\0'; 8]);

        let mut iter = chars.into_iter();
        let mut curr = iter.next().unwrap();
        let mut next = iter.next().unwrap();

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

        while let Some(&((_, _), first_char)) = self.current_chars.first() {
            if first_char.is_whitespace() {
                self.current_chars.remove(0);
            } else {
                break;
            }
        }

        match self.state {
            LexerState::Normal => {
                if c.is_numeric() && self.current_chars.is_empty() {
                    let base = match c {
                        'x' => numbers::NumberBase::Hex,
                        'o' => numbers::NumberBase::Octal,
                        'b' => numbers::NumberBase::Binary,
                        _ => numbers::NumberBase::Decimal,
                    };
                    self.state = LexerState::Number(c == '.', base);
                    self.current_chars.push(((self.cursor.line, self.cursor.col), c));
                } else if c == '/' && next == '*' {
                    self.state = LexerState::MultilineComment;
                } else if c == '/' && next == '/' {
                    self.state = LexerState::Comment;
                } else {
                    self.current_chars.push(((self.cursor.line, self.cursor.col), c));

                    if !self.check_punctuation()
                        && !self.check_type_literals()
                        && !self.check_keywords()
                        && !c.is_valid_ident_char()
                        && !self.current_chars.is_empty()
                        && self.current_chars.len() > 1
                    {
                        let ident_chars = &self.current_chars[..self.current_chars.len() - 1];
                        if ident_chars
                            .iter()
                            .all(|&(_, ch)| ch.is_valid_ident_char() || ch.is_whitespace())
                        {
                            let identifier: String = ident_chars
                                .iter()
                                .map(|(_, ch)| *ch)
                                .filter(|ch| !ch.is_whitespace())
                                .collect();

                            if !identifier.is_empty() {
                                let start_pos = ident_chars[0].0;
                                let end_pos = ident_chars[ident_chars.len() - 1].0;
                                let span = self.create_span_from_positions(start_pos, end_pos);

                                self.tokens.push(Token::new(
                                    TokenKind::Identifier(self.interner.get_or_intern(&identifier)),
                                    span,
                                ));
                            }

                            let current_char = self.current_chars.pop();
                            self.current_chars.clear();
                            if let Some(ch) = current_char {
                                self.current_chars.push(ch);
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
                    self.current_chars.push(((self.cursor.line, self.cursor.col), c));
                } else if c == '/' && self.current_chars.first().is_some_and(|(_, ch)| *ch == '*') {
                    self.current_chars.clear();
                    self.state = LexerState::Normal;
                } else if c == '\n' {
                    self.cursor.line += 1;
                    self.cursor.col = 0;
                }
            }
            _ => todo!(),
        }
    }

    fn create_span_from_positions(&self, start_pos: (usize, usize), end_pos: (usize, usize)) -> Span {
        Span::new(self.cursor.file, start_pos.0, start_pos.1, end_pos.1)
    }
}

pub trait IdentName {
    fn is_valid_ident_char(&self) -> bool;
}

impl IdentName for char {
    fn is_valid_ident_char(&self) -> bool {
        self.is_alphanumeric() || *self == '_'
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use string_interner::{backend::BucketBackend, symbol::SymbolUsize, StringInterner};
    type Interner = StringInterner<BucketBackend<SymbolUsize>>;

    #[test]
    fn test_lex_identifiers_with_keywords() {
        let mut interner = Interner::new();
        let f = interner.get_or_intern("");
        let contents = "let__ use some_ident normal";
        let mut lexer = Lexer::new(contents, interner, f);

        let chars: Vec<char> = contents.chars().chain(std::iter::once('\0')).collect();
        lexer.tokenize(chars);

        let expected_tokens = vec![
            ("Identifier(let__)".to_string(), 1, (1, 5)),
            ("Keyword(Use)".to_string(), 1, (7, 9)),
            ("Identifier(some_ident)".to_string(), 1, (11, 20)),
            ("Identifier(normal)".to_string(), 1, (22, 27)),
        ];

        let tokens = lexer
            .tokens
            .iter()
            .map(|t| {
                (
                    if let TokenKind::Identifier(identifier) = t.kind {
                        format!("Identifier({})", lexer.interner.resolve(identifier).unwrap())
                    } else {
                        format!("{:?}", t.kind)
                    },
                    t.span.line,
                    (t.span.start, t.span.end),
                )
            })
            .collect::<Vec<_>>();
        assert_eq!(tokens.len(), expected_tokens.len());

        for (i, token) in tokens.iter().enumerate() {
            assert_eq!(token, &expected_tokens[i]);
        }
    }
}
