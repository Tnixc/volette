use super::tokens::{Span, Token, TokenKind};
mod error;
use error::LexError;
use string_interner::symbol::SymbolUsize;
mod numbers;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum NumberBase {
    Decimal,
    Hex,
    Binary,
    Octal,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum LexerState {
    Number(bool, NumberBase),
    String,
    Normal,
}

#[derive(Debug)]
pub struct Lexer {
    pub tokens: Vec<Token>,
    pub state: LexerState,
    pub errors: Vec<LexError>,
    pub span: Span,
    pub current_str: String,
}

impl Lexer {
    pub fn new(str: &str, path: SymbolUsize) -> Self {
        Lexer {
            tokens: Vec::with_capacity(str.len() / 5),
            state: LexerState::Normal,
            errors: Vec::new(),
            span: Span::new(path, 1, 0, 0),
            current_str: String::new(),
        }
    }

    pub fn next(&mut self, c: char) -> () {
        self.span.end += 1;

        match self.state {
            LexerState::Normal => {
                if c.is_numeric() {
                    self.state = LexerState::Number(false, NumberBase::Decimal);
                    self.span.start = self.span.end;
                    self.current_str.push(c);
                } else if c == '"' {
                    self.state = LexerState::String;
                } else {
                    if c == '\n' {
                        self.span.line += 1;
                    }
                }
            }
            LexerState::Number(float, base) => {
                if self.current_str == "0" && (c == 'x' || c == 'b' || c == 'o') {
                    self.state = LexerState::Number(
                        float,
                        match c {
                            'x' => NumberBase::Hex,
                            'b' => NumberBase::Binary,
                            'o' => NumberBase::Octal,
                            _ => unreachable!(),
                        },
                    );
                } else if c.is_numeric() {
                    self.current_str.push(c);
                } else if c == '.' {
                    self.state = LexerState::Number(true, base);
                    self.current_str.push(c);
                } else if c == '_' {
                    // ignore
                } else {
                    // It's a number!
                    if float {
                        if base != NumberBase::Decimal {
                            self.errors.push(LexError::NonIntegerBase {
                                base,
                                span: self.span.clone(),
                            });
                        } else if let Ok(n) = self.parse_float() {
                            self.tokens
                                .push(Token::new(TokenKind::FloatLiteral(n), self.span.clone()));
                        }
                    } else {
                        self.span.end -= 1;
                        if let Ok(n) = self.parse_int_with_base(base) {
                            self.tokens
                                .push(Token::new(TokenKind::IntLiteral(n), self.span.clone()));
                        }
                    }
                    self.state = LexerState::Normal;
                    self.current_str.clear();
                }
            }
            _ => unreachable!(),
        }
    }
}
