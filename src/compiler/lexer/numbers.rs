use crate::compiler::tokens::{Token, TokenKind};

use super::{error::LexError, Lexer, LexerState, NumberBase};

impl Lexer {
    pub fn lex_number(&mut self, c: char, _window: &str, float: bool, negative: bool, base: NumberBase) -> () {
        if self.current_str == "0" && (c == 'x' || c == 'b' || c == 'o') {
            self.state = LexerState::Number(
                float,
                negative,
                match c {
                    'x' => NumberBase::Hex,
                    'b' => NumberBase::Binary,
                    'o' => NumberBase::Octal,
                    _ => unreachable!(),
                },
            );
        } else if c == '.' {
            self.state = LexerState::Number(true, negative, base);
            self.current_str.push(c);
        } else if c == '_' {
            // ignore
        } else if base == NumberBase::Hex && c.is_numeric()
            || ['a', 'b', 'c', 'd', 'e', 'f'].contains(&c.to_ascii_lowercase())
        {
            self.current_str.push(c);
        } else if base == NumberBase::Binary && ['0', '1'].contains(&c) {
            self.current_str.push(c);
        } else if base == NumberBase::Octal && ['0', '1', '2', '3', '4', '5', '6', '7'].contains(&c) {
            self.current_str.push(c);
        } else if base == NumberBase::Decimal && c.is_numeric() {
            self.current_str.push(c);
        } else {
            // It's a number!
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
                self.span.end -= 1;
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

        let chars: Vec<char> = contents.chars().chain(std::iter::once(' ')).collect();
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
        let contents = "0x12.3g";
        let mut lexer = Lexer::new(contents, file);

        let chars: Vec<char> = contents.chars().chain(std::iter::once(' ')).collect();
        for (i, &ch) in chars.iter().enumerate() {
            lexer.next(ch, &contents[i..]);
        }

        assert_eq!(lexer.errors.len(), 1);
    }
}
