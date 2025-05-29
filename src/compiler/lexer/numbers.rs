use crate::compiler::tokens::{Token, TokenKind};

use super::{error::LexError, Lexer, LexerState};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum NumberBase {
    Decimal,
    Hex,
    Binary,
    Octal,
}

impl Lexer {
    pub fn lex_number(&mut self, c: char, float: bool, negative: bool, base: NumberBase) -> bool {
        if self.current_str == "0" && (c == 'x' || c == 'b' || c == 'o') {
            self.state = LexerState::Number(
                float,
                negative,
                match c {
                    'x' => NumberBase::Hex,
                    'b' => NumberBase::Binary,
                    'o' => NumberBase::Octal,
                    _ => return true,
                },
            );
            return true;
        }

        let is_valid_char = if c == '.' && !float && base == NumberBase::Decimal {
            self.state = LexerState::Number(true, negative, base);
            true
        } else if c == '_' {
            true
        } else if base == NumberBase::Octal && c.is_digit(8) {
            true
        } else if base == NumberBase::Binary && c.is_digit(2) {
            true
        } else if base == NumberBase::Hex && c.is_ascii_hexdigit() {
            true
        } else if base == NumberBase::Decimal && c.is_numeric() {
            true
        } else {
            false
        };

        if is_valid_char {
            if c != '_' {
                self.current_str.push(c);
            }
            true
        } else {
            self.push_number(float, negative, base);
            self.cursor.col -= 1;
            false
        }
    }

    fn push_number(&mut self, float: bool, negative: bool, base: NumberBase) {
        let span_length = self.cursor.col - self.token_start;
        let span = self.create_span(self.token_start, self.token_start + span_length);

        if float {
            if base != NumberBase::Decimal {
                self.errors.push(LexError::NonIntegerBase {
                    base,
                    span: span.clone(),
                });
            } else if let Ok(mut n) = self.lex_float() {
                if negative {
                    n = -n;
                }
                self.tokens.push(Token::new(TokenKind::FloatLiteral(n), span));
            }
        } else if let Ok(mut n) = self.lex_int(base) {
            if negative {
                n = -n;
            }
            self.tokens.push(Token::new(TokenKind::IntLiteral(n), span));
        }
        self.state = LexerState::Normal;
        self.current_str.clear();
    }

    pub fn lex_float(&mut self) -> Result<f64, ()> {
        self.current_str.parse::<f64>().map_err(|e| {
            let span = self.create_span(self.token_start, self.cursor.col);
            self.errors.push(LexError::InvalidFloat {
                value: self.current_str.clone(),
                span,
                source: e,
            });
        })
    }

    pub fn lex_int(&mut self, base: NumberBase) -> Result<i64, ()> {
        let radix = match base {
            NumberBase::Decimal => 10,
            NumberBase::Hex => 16,
            NumberBase::Binary => 2,
            NumberBase::Octal => 8,
        };

        let result = if base == NumberBase::Decimal {
            self.current_str.parse::<i64>()
        } else {
            i64::from_str_radix(&self.current_str, radix)
        };

        result.map_err(|e| {
            let span = self.create_span(self.token_start, self.cursor.col);
            self.errors.push(LexError::InvalidInteger {
                value: self.current_str.clone(),
                span,
                source: e,
            });
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::tokens::TokenKind;
    use string_interner::{backend::BucketBackend, symbol::SymbolUsize, StringInterner};
    type Interner = StringInterner<BucketBackend<SymbolUsize>>;

    #[test]

    fn test_lex_numbers() {
        let mut interner = Interner::new();
        let file = interner.get_or_intern("");
        let contents = "-123 2 -0xff 3.14 -2.71 0b1111 0o234";
        let mut lexer = Lexer::new(contents, file);

        let chars: Vec<char> = contents.chars().chain(std::iter::once('\0')).collect();
        lexer.tokenize(chars);

        assert_eq!(lexer.tokens.len(), 7);
        assert_eq!(lexer.tokens[0].kind, TokenKind::IntLiteral(-123));
        assert_eq!(lexer.tokens[1].kind, TokenKind::IntLiteral(2));
        assert_eq!(lexer.tokens[2].kind, TokenKind::IntLiteral(-0xff));
        assert_eq!(lexer.tokens[3].kind, TokenKind::FloatLiteral(3.14));
        assert_eq!(lexer.tokens[4].kind, TokenKind::FloatLiteral(-2.71));
        assert_eq!(lexer.tokens[5].kind, TokenKind::IntLiteral(0b1111));
        assert_eq!(lexer.tokens[6].kind, TokenKind::IntLiteral(0o234));
    }
}
