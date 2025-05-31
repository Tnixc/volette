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
    pub fn lex_number(&mut self, c: char, float: bool, base: NumberBase) -> bool {
        let current_string = self.current_chars.iter().map(|(_, ch)| *ch).collect::<String>();

        if current_string == "0" && (c == 'x' || c == 'b' || c == 'o') {
            self.current_chars.push_back(((self.cursor.line, self.cursor.col), c));
            self.state = LexerState::Number(
                float,
                match c {
                    'x' => NumberBase::Hex,
                    'b' => NumberBase::Binary,
                    'o' => NumberBase::Octal,
                    _ => return true,
                },
            );
            return true;
        }

        let is_valid_char = if c == '.' && !float {
            self.state = LexerState::Number(true, base);
            true
        } else {
            c == '_'
                || base == NumberBase::Octal && c.is_digit(8)
                || base == NumberBase::Binary && c.is_digit(2)
                || base == NumberBase::Hex && c.is_ascii_hexdigit()
                || base == NumberBase::Decimal && c.is_ascii_digit()
        };

        if is_valid_char {
            self.current_chars.push_back(((self.cursor.line, self.cursor.col), c));
            true
        } else {
            self.push_number(float, base);
            self.state = LexerState::Normal;
            false
        }
    }

    fn push_number(&mut self, float: bool, base: NumberBase) {
        if self.current_chars.is_empty() {
            return;
        }

        let start_pos = self.current_chars[0].0;
        let end_pos = self.current_chars.back().map(|(pos, _)| *pos).unwrap_or(start_pos);
        let span = self.create_span(start_pos.0, start_pos.1, end_pos.0, end_pos.1);

        if float {
            if base != NumberBase::Decimal {
                self.errors.push(LexError::NonDecimalFloat {
                    span: span.to_display(&self.interner),
                });
            } else if let Ok(n) = self.lex_float() {
                self.tokens.push(Token::new(TokenKind::FloatLiteral(n), span));
            }
        } else if let Ok(n) = self.lex_int(base) {
            self.tokens.push(Token::new(TokenKind::IntLiteral(n), span));
        }

        self.state = LexerState::Normal;
        self.current_chars.drain(..self.current_chars.len()).for_each(|_| {});
    }

    pub fn lex_float(&mut self) -> Result<f64, LexError> {
        let current_string = self.current_chars.iter().map(|(_, ch)| *ch).collect::<String>();
        let filtered_str: String = current_string.chars().filter(|&c| c != '_').collect();

        filtered_str.parse::<f64>().map_err(|e| {
            let start_pos = self
                .current_chars
                .front()
                .map(|(pos, _)| *pos)
                .unwrap_or((self.cursor.line, self.cursor.col));
            let end_pos = self.current_chars.back().map(|(pos, _)| *pos).unwrap_or(start_pos);
            let span = self.create_span(start_pos.0, start_pos.1, end_pos.0, end_pos.1);

            let err = LexError::InvalidFloat {
                value: filtered_str.clone(),
                span: span.to_display(&self.interner),
                source: e,
            };
            self.errors.push(err.clone());
            err
        })
    }

    pub fn lex_int(&mut self, base: NumberBase) -> Result<i64, LexError> {
        let radix = match base {
            NumberBase::Decimal => 10,
            NumberBase::Hex => 16,
            NumberBase::Binary => 2,
            NumberBase::Octal => 8,
        };

        let current_string = self.current_chars.iter().map(|(_, ch)| *ch).collect::<String>();
        let filtered_str: String = current_string.chars().filter(|&c| c != '_').collect();

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

        result.map_err(|e| {
            let start_pos = self
                .current_chars
                .front()
                .map(|(pos, _)| *pos)
                .unwrap_or((self.cursor.line, self.cursor.col));
            let end_pos = self.current_chars.back().map(|(pos, _)| *pos).unwrap_or(start_pos);
            let span = self.create_span(start_pos.0, start_pos.1, end_pos.0, end_pos.1);

            let err = LexError::InvalidInteger {
                value: filtered_str.clone(),
                span: span.to_display(&self.interner),
                source: e,
            };
            self.errors.push(err.clone());
            err
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::tokens::Punctuation::*;
    use crate::compiler::tokens::TokenKind::*;
    use string_interner::{backend::BucketBackend, symbol::SymbolUsize, StringInterner};
    type Interner = StringInterner<BucketBackend<SymbolUsize>>;

    #[test]
    fn test_lex_numbers() {
        let mut interner = Interner::new();
        let file = interner.get_or_intern("");
        let contents = "123 + 2 0xff 3.14 2.71 0 0o234";
        let mut lexer = Lexer::new(contents, interner, file);

        let chars: Vec<char> = contents.chars().chain(std::iter::once('\0')).collect();
        lexer.tokenize(chars);

        let expected_tokens = vec![
            (IntLiteral(123), 1, (1, 3)),
            (Punctuation(Plus), 1, (5, 5)),
            (IntLiteral(2), 1, (7, 7)),
            (IntLiteral(255), 1, (9, 12)), // 0xff = 255
            (FloatLiteral(3.14), 1, (14, 17)),
            (FloatLiteral(2.71), 1, (19, 22)),
            (IntLiteral(0), 1, (24, 24)),
            (IntLiteral(156), 1, (26, 30)), // 0o234 = 156
        ];

        assert_eq!(lexer.tokens.len(), expected_tokens.len());
        let tokens = lexer
            .tokens
            .iter()
            .map(|t| (t.kind, t.span.start.0, (t.span.start.1, t.span.end.1)))
            .collect::<Vec<_>>();
        for (i, token) in tokens.iter().enumerate() {
            assert_eq!(token, &expected_tokens[i]);
        }
    }
}
