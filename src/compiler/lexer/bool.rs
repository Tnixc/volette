use crate::compiler::tokens::{Token, TokenKind};

use super::Lexer;
use crate::compiler::lexer::LexedChar;

impl<'a> Lexer<'a> {
    fn push_bool(&mut self, bool: bool, chars_consumed: usize) {
        if chars_consumed == 0 || self.current_chars.is_empty() {
            return;
        }

        let start_pos = self.current_chars[0].0;
        let end_pos = if chars_consumed <= self.current_chars.len() {
            self.current_chars[chars_consumed - 1].0
        } else {
            self.current_chars.back().map(|c| c.0).unwrap_or(start_pos)
        };

        let span = self.create_span(start_pos.0, start_pos.1, end_pos.0, end_pos.1);
        self.tokens.push(Token::new(TokenKind::BoolLiteral(bool), span));
    }

    fn push_nil(&mut self, chars_consumed: usize) {
        if chars_consumed == 0 || self.current_chars.is_empty() {
            return;
        }

        let start_pos = self.current_chars[0].0;
        let end_pos = if chars_consumed <= self.current_chars.len() {
            self.current_chars[chars_consumed - 1].0
        } else {
            self.current_chars.back().map(|c| c.0).unwrap_or(start_pos)
        };

        let span = self.create_span(start_pos.0, start_pos.1, end_pos.0, end_pos.1);
        self.tokens.push(Token::new(TokenKind::NilLiteral, span));
    }

    pub fn check_bool(&mut self) -> bool {
        const PATTERNS: &[(&str, bool)] = &[("true", true), ("false", false)];

        for &(pattern, value) in PATTERNS {
            let pattern_len = pattern.len();

            if self.current_chars.len() >= pattern_len {
                let matches = pattern
                    .bytes()
                    .enumerate()
                    .all(|(i, expected)| self.current_chars[i].1 as u8 == expected);

                if matches {
                    let has_valid_boundary =
                        self.current_chars.len() == pattern_len || !self.current_chars[pattern_len].1.is_valid_ident_char();

                    if has_valid_boundary {
                        self.push_bool(value, pattern_len);
                        self.current_chars.drain(..pattern_len);
                        return true;
                    }
                }
            }
        }

        false
    }

    pub fn check_nil(&mut self) -> bool {
        const PATTERN: &str = "nil";
        let pattern_len = PATTERN.len();

        if self.current_chars.len() >= pattern_len {
            let matches = PATTERN
                .bytes()
                .enumerate()
                .all(|(i, expected)| self.current_chars[i].1 as u8 == expected);

            if matches {
                let has_valid_boundary =
                    self.current_chars.len() == pattern_len || !self.current_chars[pattern_len].1.is_valid_ident_char();

                if has_valid_boundary {
                    self.push_nil(pattern_len);
                    self.current_chars.drain(..pattern_len);
                    return true;
                }
            }
        }

        false
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use string_interner::{StringInterner, backend::BucketBackend, symbol::SymbolUsize};
    type Interner = StringInterner<BucketBackend<SymbolUsize>>;
    use crate::compiler::tokens::TokenKind::*;

    #[test]
    fn test_lex_bool() {
        let mut interner = Interner::new();
        let file = interner.get_or_intern("");
        let contents = r#"true false"#;
        let mut lexer = Lexer::new(&mut interner, file);

        let chars: Vec<char> = contents.chars().chain(std::iter::once('\0')).collect();
        lexer.tokenize(chars);

        let expected_tokens = vec![(BoolLiteral(true), 1, (1, 4)), (BoolLiteral(false), 1, (6, 10))];

        let tokens = lexer
            .tokens
            .iter()
            .skip(1)
            .map(|t| (t.kind, t.span.start.0, (t.span.start.1, t.span.end.1)))
            .collect::<Vec<_>>();
        for (i, token) in tokens.iter().enumerate() {
            assert_eq!(token, &expected_tokens[i]);
        }
    }
}
