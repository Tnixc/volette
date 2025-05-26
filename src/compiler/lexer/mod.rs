use super::tokens::{Span, Token, TokenKind};
mod error;
use error::LexError;
use string_interner::symbol::SymbolUsize;

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
                            let err = LexError::NonIntegerBase {
                                base: base,
                                span: self.span.clone(),
                            };
                            self.errors.push(err);
                            ()
                        }
                        let n = self.current_str.parse::<f64>().map_err(|e| {
                            let e = LexError::InvalidFloat {
                                value: self.current_str.clone(),
                                span: self.span.clone(),
                                source: e,
                            };
                            self.errors.push(e);
                            ()
                        });
                        if let Ok(n) = n {
                            self.tokens
                                .push(Token::new(TokenKind::FloatLiteral(n), self.span.clone()));
                        }
                    } else {
                        self.span.end -= 1;
                        let n = match base {
                            NumberBase::Decimal => self.current_str.parse::<i64>().map_err(|e| {
                                let err = LexError::InvalidInteger {
                                    value: self.current_str.clone(),
                                    span: self.span.clone(),
                                    source: e,
                                };
                                self.errors.push(err);
                                ()
                            }),
                            NumberBase::Hex => {
                                i64::from_str_radix(&self.current_str, 16).map_err(|e| {
                                    let err = LexError::InvalidInteger {
                                        value: self.current_str.clone(),
                                        span: self.span.clone(),
                                        source: e,
                                    };
                                    self.errors.push(err);
                                    ()
                                })
                            }

                            NumberBase::Binary => i64::from_str_radix(&self.current_str, 2)
                                .map_err(|e| {
                                    let err = LexError::InvalidInteger {
                                        value: self.current_str.clone(),
                                        span: self.span.clone(),
                                        source: e,
                                    };
                                    self.errors.push(err);
                                    ()
                                }),

                            NumberBase::Octal => {
                                i64::from_str_radix(&self.current_str, 8).map_err(|e| {
                                    let err = LexError::InvalidInteger {
                                        value: self.current_str.clone(),
                                        span: self.span.clone(),
                                        source: e,
                                    };
                                    self.errors.push(err);
                                    ()
                                })
                            }
                        };
                        if let Ok(n) = n {
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
