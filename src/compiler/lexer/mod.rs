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
    Number(bool, bool, NumberBase),
    String,
    Normal,
    Comment,
    MultilineComment,
    End,
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

    pub fn next(&mut self, c: char, window: &str) -> () {
        self.span.end += 1;

        match self.state {
            LexerState::Normal => {
                if c.is_numeric() || c == '-' {
                    let negative = c == '-';
                    self.state = LexerState::Number(false, negative, NumberBase::Decimal);
                    self.span.start = self.span.end;
                    if !negative {
                        self.current_str.push(c);
                    }
                } else if c == '"' {
                    self.state = LexerState::String;
                } else {
                    if c == '\n' {
                        self.span.line += 1;
                    }
                }
            }
            LexerState::Number(float, negative, base) => {
                self.lex_number(c, window, float, negative, base);
            }
            _ => todo!(),
        }
    }
}
