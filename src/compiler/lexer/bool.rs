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

    pub fn check_bool(&mut self) -> bool {
        macro_rules! check_bool {
            ($str:expr, $bool:expr) => {{
                let str_chars: Vec<char> = $str.chars().collect();

                if self.current_chars.len() > str_chars.len() {
                    let matches = self
                        .current_chars
                        .iter()
                        .take(str_chars.len())
                        .zip(str_chars.iter())
                        .all(|((_, char_from_input), &expected_char)| *char_from_input == expected_char);

                    let has_valid_boundary = if self.current_chars.len() > str_chars.len() {
                        !self.current_chars[str_chars.len()].1.is_valid_ident_char()
                    } else {
                        true
                    };

                    if matches && has_valid_boundary {
                        self.push_bool($bool, str_chars.len());
                        self.current_chars.drain(..str_chars.len());
                        return true;
                    }
                }
                false
            }};
        }

        check_bool!("true", true);
        check_bool!("false", false);

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
