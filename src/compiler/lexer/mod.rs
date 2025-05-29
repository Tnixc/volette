use super::tokens::{Span, Token};
mod error;
use error::LexError;
use string_interner::symbol::SymbolUsize;
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
}

impl Lexer {
    pub fn new(str: &str, path: SymbolUsize) -> Self {
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
            token_start: 0,
        }
    }

    pub fn tokenize(&mut self, chars: Vec<char>) {
        let mut iter = chars.iter().enumerate();

        while let Some((i, &ch)) = iter.next() {
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
                if c.is_numeric() || (c == '-' && window.get(1).is_some_and(|&x| x.is_digit(10))) {
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
                    self.check_punctuation();
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
