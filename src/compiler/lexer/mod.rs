use crate::compiler::tokens::TokenKind;

use super::tokens::{Span, Token};
mod error;
use error::LexError;
use string_interner::{backend::BucketBackend, symbol::SymbolUsize, StringInterner};
mod keywords;
mod numbers;
mod punctuation;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum LexerState {
    Number(bool, bool, numbers::NumberBase),
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

pub struct Lexer {
    pub tokens: Vec<Token>,
    pub state: LexerState,
    pub errors: Vec<LexError>,
    pub cursor: Cursor,
    pub current_str: String,
    pub token_start: usize,
    pub interner: StringInterner<BucketBackend<SymbolUsize>>,
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
            current_str: String::with_capacity(64),
            token_start: 0, // TODO: refractor number to not use this
            interner,
        }
    }

    pub fn tokenize(&mut self, chars: Vec<char>) {
        let iter = chars.iter().enumerate();

        for (i, &ch) in iter {
            let window_end = (i + 10).min(chars.len());
            let window = &chars[i..window_end];

            self.next(ch, window);
        }
    }

    pub fn next(&mut self, c: char, window: &[char]) {
        self.cursor.col += 1;
        self.current_str
            .drain(..(self.current_str.len() - self.current_str.trim_start().len()));

        match self.state {
            LexerState::Normal => {
                if c.is_numeric() || (c == '-' && window.get(1).is_some_and(|&x| x.is_ascii_digit())) {
                    self.token_start = self.cursor.col;
                    let negative = c == '-';
                    self.state = LexerState::Number(false, negative, numbers::NumberBase::Decimal);
                    if !negative {
                        self.current_str.push(c);
                    }
                } else if c == '\n' {
                    self.cursor.line += 1;
                    self.cursor.col = 0;
                } else {
                    self.current_str.push(c);
                    self.token_start = self.cursor.col;

                    if !self.check_punctuation()
                        && !self.check_keywords()
                        && !c.is_valid_ident_char()
                        && !self.current_str.trim().is_empty()
                    {
                        // identifier
                        let identifier = self
                            .current_str
                            .drain(0..self.current_str.len() - 1)
                            .collect::<String>();
                        if !identifier.is_empty() {
                            self.tokens.push(Token::new(
                                TokenKind::Identifier(self.interner.get_or_intern(identifier.as_str())),
                                self.create_span(self.cursor.col - identifier.len(), self.cursor.col - 1),
                            ));
                        }
                    }
                }
            }
            LexerState::Number(float, negative, base) => {
                if !self.lex_number(c, float, negative, base) {
                    self.next(c, window);
                }
            }
            _ => todo!(),
        }
    }

    fn create_span(&self, start: usize, end: usize) -> Span {
        Span::new(self.cursor.file, self.cursor.line, start, end)
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
            ("let__".to_string(), 1, (1, 5)),
            ("Keyword(Use)".to_string(), 1, (7, 9)),
            ("some_ident".to_string(), 1, (11, 20)),
            ("normal".to_string(), 1, (22, 27)),
        ];
        assert_eq!(lexer.tokens.len(), expected_tokens.len());
        let tokens = lexer
            .tokens
            .iter()
            .map(|t| {
                (
                    if let TokenKind::Identifier(identifier) = t.kind {
                        format!("{}", lexer.interner.resolve(identifier).unwrap())
                    } else {
                        format!("{:?}", t.kind)
                    },
                    t.span.line,
                    (t.span.start, t.span.end),
                )
            })
            .collect::<Vec<_>>();
        for (i, token) in tokens.iter().enumerate() {
            assert_eq!(token, &expected_tokens[i]);
        }
    }
}
