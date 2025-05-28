use super::tokens::{Span, Token};
mod error;
use error::LexError;
use string_interner::symbol::SymbolUsize;
mod numbers;
mod punctuation;

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

    pub fn tokenize(&mut self, chars: Vec<char>) {
        let mut iter = chars.iter().enumerate();

        while let Some((i, &ch)) = iter.next() {
            let window_end = (i + 10).min(chars.len());
            let window = &chars[i..window_end];

            let skip_next = self.next(ch, window);
            if skip_next {
                iter.next();
            }
        }
    }

    pub fn next(&mut self, c: char, window: &[char]) -> bool {
        self.span.end += 1;

        match self.state {
            LexerState::Normal => {
                if c.is_numeric() || (c == '-' && window.get(1).is_some_and(|&x| x.is_numeric())) {
                    let negative = c == '-';
                    self.state = LexerState::Number(false, negative, NumberBase::Decimal);
                    self.span.start = self.span.end;
                    if !negative {
                        self.current_str.push(c);
                    }
                    false
                } else if c == '\n' {
                    self.span.line += 1;
                    self.span.start = 0;
                    self.span.end = 0;
                    false
                } else {
                    self.lex_punctuation(c, window)
                }
            }
            LexerState::Number(float, negative, base) => {
                self.lex_number(c, window, float, negative, base);
                false
            }
            _ => todo!(),
        }
    }
}
