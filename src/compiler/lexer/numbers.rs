use crate::compiler::tokens::{Token, TokenKind};

use super::{error::LexError, Lexer, LexerState, NumberBase};

impl Lexer {
    pub fn lex_number(&mut self, c: char, window: &str, float: bool, negative: bool, base: NumberBase) -> () {
        if self.current_str == "0" && (c == 'x' || c == 'b' || c == 'o') {
            self.state = LexerState::Number(
                float,
                negative,
                match c {
                    'x' => NumberBase::Hex,
                    'b' => NumberBase::Binary,
                    'o' => NumberBase::Octal,
                    _ => return,
                },
            );
            return;
        }

        let should_add_char = if c == '.' && !float && base == NumberBase::Decimal {
            self.state = LexerState::Number(true, negative, base);
            true
        } else if c == '_' {
            false
        } else if base == NumberBase::Hex
            && (c.is_numeric() || ['a', 'b', 'c', 'd', 'e', 'f'].contains(&c.to_ascii_lowercase()))
        {
            true
        } else if base == NumberBase::Binary && ['0', '1'].contains(&c) {
            true
        } else if base == NumberBase::Octal && ['0', '1', '2', '3', '4', '5', '6', '7'].contains(&c) {
            true
        } else if base == NumberBase::Decimal && c.is_numeric() {
            true
        } else {
            false
        };

        if should_add_char {
            self.current_str.push(c);

            let next_char = window.chars().nth(1);
            let should_end = match next_char {
                Some(next_c) => {
                    if c == '0' && (next_c == 'x' || next_c == 'b' || next_c == 'o') {
                        return;
                    }
                    if base != NumberBase::Decimal && next_c == '.' {
                        let err = LexError::NonIntegerBase {
                            base: base,
                            span: self.span.clone(),
                        };
                        self.errors.push(err);
                        return;
                    }
                    match base {
                        NumberBase::Hex => {
                            !(c.is_numeric() || ['a', 'b', 'c', 'd', 'e', 'f', '_'].contains(&c.to_ascii_lowercase()))
                        }
                        NumberBase::Binary => !['0', '1', '_'].contains(&next_c),
                        NumberBase::Octal => !['0', '1', '2', '3', '4', '5', '6', '7', '_'].contains(&next_c),
                        NumberBase::Decimal => {
                            if float {
                                !next_c.is_numeric() && next_c != '_'
                            } else {
                                !next_c.is_numeric() && next_c != '_' && next_c != '.'
                            }
                        }
                    }
                }
                None => true,
            };

            if should_end {
                self.finalize_number(float, negative, base);
            }
        } else {
            self.finalize_number(float, negative, base);
            self.span.end -= 1;
        }
    }

    fn finalize_number(&mut self, float: bool, negative: bool, base: NumberBase) {
        if float {
            if base != NumberBase::Decimal {
                self.errors.push(LexError::NonIntegerBase {
                    base,
                    span: self.span.clone(),
                });
            } else if let Ok(mut n) = self.lex_float() {
                if negative {
                    n = -n;
                }
                self.tokens
                    .push(Token::new(TokenKind::FloatLiteral(n), self.span.clone()));
            }
        } else {
            if let Ok(mut n) = self.lex_int(base) {
                if negative {
                    n = -n;
                }
                self.tokens
                    .push(Token::new(TokenKind::IntLiteral(n), self.span.clone()));
            }
        }
        self.state = LexerState::Normal;
        self.current_str.clear();
    }

    pub fn lex_float(&mut self) -> Result<f64, ()> {
        self.current_str.parse::<f64>().map_err(|e| {
            self.errors.push(LexError::InvalidFloat {
                value: self.current_str.clone(),
                span: self.span.clone(),
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
            self.errors.push(LexError::InvalidInteger {
                value: self.current_str.clone(),
                span: self.span.clone(),
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
        for (i, &ch) in chars.iter().enumerate() {
            lexer.next(ch, &contents[i..]);
        }

        assert_eq!(lexer.tokens.len(), 7);
        assert_eq!(lexer.tokens[0].kind, TokenKind::IntLiteral(-123));
        assert_eq!(lexer.tokens[1].kind, TokenKind::IntLiteral(2));
        assert_eq!(lexer.tokens[2].kind, TokenKind::IntLiteral(-0xff));
        assert_eq!(lexer.tokens[3].kind, TokenKind::FloatLiteral(3.14));
        assert_eq!(lexer.tokens[4].kind, TokenKind::FloatLiteral(-2.71));
        assert_eq!(lexer.tokens[5].kind, TokenKind::IntLiteral(0b1111));
        assert_eq!(lexer.tokens[6].kind, TokenKind::IntLiteral(0o234));
    }

    #[test]
    fn test_lex_float_with_base_error() {
        let mut interner = Interner::new();
        let file = interner.get_or_intern("");
        let contents = "0x12.3";
        let mut lexer = Lexer::new(contents, file);

        let chars: Vec<char> = contents.chars().chain(std::iter::once('\0')).collect();
        for (i, &ch) in chars.iter().enumerate() {
            lexer.next(ch, &contents[i..]);
        }

        assert_eq!(lexer.errors.len(), 1);
    }
}
