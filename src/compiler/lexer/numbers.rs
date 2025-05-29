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
            self.current_str.push(c);
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
        } else {
            c == '_'
                || base == NumberBase::Octal && c.is_digit(8)
                || base == NumberBase::Binary && c.is_digit(2)
                || base == NumberBase::Hex && c.is_ascii_hexdigit()
                || base == NumberBase::Decimal && c.is_ascii_digit()
        };

        if is_valid_char {
            self.current_str.push(c);
            true
        } else {
            self.push_number(float, negative, base);
            self.cursor.col -= 1;
            false
        }
    }

    fn push_number(&mut self, float: bool, negative: bool, base: NumberBase) {
        let number_str = if negative {
            format!("-{}", &self.current_str)
        } else {
            self.current_str.clone()
        };

        let span_length = number_str.len();
        let span = self.create_span(self.cursor.col - span_length, self.cursor.col - 1);

        if float {
            if base != NumberBase::Decimal {
                self.errors.push(LexError::NonIntegerBase {
                    base,
                    span: span.clone(),
                });
            } else if let Ok(n) = self.lex_float(negative) {
                self.tokens.push(Token::new(TokenKind::FloatLiteral(n), span));
            }
        } else if let Ok(n) = self.lex_int(base, negative) {
            self.tokens.push(Token::new(TokenKind::IntLiteral(n), span));
        }
        self.state = LexerState::Normal;
        self.current_str.clear();
    }

    pub fn lex_float(&mut self, negative: bool) -> Result<f64, ()> {
        let filtered_str: String = self.current_str.chars().filter(|&c| c != '_').collect();
        let result = filtered_str.parse::<f64>().map_err(|e| {
            let span_length = if negative {
                self.current_str.len() + 1
            } else {
                self.current_str.len()
            };
            let span = self.create_span(self.cursor.col - span_length, self.cursor.col - 1);
            self.errors.push(LexError::InvalidFloat {
                value: if negative {
                    format!("-{}", filtered_str)
                } else {
                    filtered_str
                },
                span,
                source: e,
            });
        });

        if negative {
            result.map(|n| -n)
        } else {
            result
        }
    }

    pub fn lex_int(&mut self, base: NumberBase, negative: bool) -> Result<i64, ()> {
        let radix = match base {
            NumberBase::Decimal => 10,
            NumberBase::Hex => 16,
            NumberBase::Binary => 2,
            NumberBase::Octal => 8,
        };

        let filtered_str: String = self.current_str.chars().filter(|&c| c != '_').collect();

        let parse_str = if base == NumberBase::Decimal {
            filtered_str.clone()
        } else {
            filtered_str.get(2..).unwrap_or("").to_string()
        };

        let result = if base == NumberBase::Decimal {
            parse_str.parse::<i64>()
        } else {
            i64::from_str_radix(&parse_str, radix)
        };

        let final_result = result.map_err(|e| {
            let span_length = if negative {
                self.current_str.len() + 1
            } else {
                self.current_str.len()
            };
            let span = self.create_span(self.cursor.col - span_length, self.cursor.col - 1);
            self.errors.push(LexError::InvalidInteger {
                value: if negative {
                    format!("-{}", filtered_str)
                } else {
                    filtered_str
                },
                span,
                source: e,
            });
        });

        if negative {
            final_result.map(|n| -n)
        } else {
            final_result
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::tokens::TokenKind::*;
    use string_interner::{backend::BucketBackend, symbol::SymbolUsize, StringInterner};
    type Interner = StringInterner<BucketBackend<SymbolUsize>>;

    #[test]

    fn test_lex_numbers() {
        let mut interner = Interner::new();
        let file = interner.get_or_intern("");
        let contents = "-123 2 -0xff 3.14 -2.71 0b1111 0o234";
        let mut lexer = Lexer::new(contents, interner, file);

        let chars: Vec<char> = contents.chars().chain(std::iter::once('\0')).collect();
        lexer.tokenize(chars);

        let expected_tokens = vec![
            (IntLiteral(-123), 1, (1, 4)),
            (IntLiteral(2), 1, (6, 6)),
            (IntLiteral(-0xff), 1, (8, 12)),
            (FloatLiteral(3.14), 1, (14, 17)),
            (FloatLiteral(-2.71), 1, (19, 23)),
            (IntLiteral(0b1111), 1, (25, 30)),
            (IntLiteral(0o234), 1, (32, 36)),
        ];

        assert_eq!(lexer.tokens.len(), expected_tokens.len());
        let tokens = lexer
            .tokens
            .iter()
            .map(|t| (t.kind, t.span.line, (t.span.start, t.span.end)))
            .collect::<Vec<_>>();
        for (i, token) in tokens.iter().enumerate() {
            assert_eq!(token, &expected_tokens[i]);
        }
    }
}
