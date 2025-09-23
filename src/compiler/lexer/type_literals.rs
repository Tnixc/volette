use crate::compiler::tokens::{Token, TokenKind};

use super::Lexer;
use crate::compiler::lexer::LexedChar;

impl<'a> Lexer<'a> {
    fn push_type_literal(&mut self, type_literal: crate::compiler::tokens::PrimitiveTypes, chars_consumed: usize) {
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
        self.tokens.push(Token::new(TokenKind::TypeLiteral(type_literal), span));
    }

    pub fn check_type_literals(&mut self) -> bool {
        use crate::compiler::tokens::PrimitiveTypes::*;

        const PATTERNS: &[(&str, crate::compiler::tokens::PrimitiveTypes)] = &[
            ("usize", Usize),
            ("isize", Isize),
            ("bool", Bool),
            ("i16", I16),
            ("i32", I32),
            ("i64", I64),
            ("u16", U16),
            ("u32", U32),
            ("u64", U64),
            ("f32", F32),
            ("f64", F64),
            ("Nil", Nil),
            ("i8", I8),
            ("u8", U8),
        ];

        for &(pattern, type_literal) in PATTERNS {
            let pattern_len = pattern.len();

            if self.current_chars.len() >= pattern_len {
                let matches = pattern
                    .bytes()
                    .enumerate()
                    .all(|(i, expected)| self.current_chars[i].1 as u8 == expected);

                if matches {
                    let has_valid_boundary = if self.current_chars.len() > pattern_len {
                        !self.current_chars[pattern_len].1.is_valid_ident_char()
                    } else {
                        true
                    };

                    if has_valid_boundary {
                        self.push_type_literal(type_literal, pattern_len);
                        self.current_chars.drain(..pattern_len);
                        return true;
                    }
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
    use crate::compiler::tokens::PrimitiveTypes::*;
    use crate::compiler::tokens::TokenKind::*;

    #[test]
    fn test_lex_types() {
        let mut interner = Interner::new();
        let file = interner.get_or_intern("");
        let contents = r#"i8 i16 i32 i64 u8 u16 u32 u64 f32 f64 bool Nil"#;
        let mut lexer = Lexer::new(&mut interner, file);

        let chars: Vec<char> = contents.chars().chain(std::iter::once('\0')).collect();
        lexer.tokenize(chars);

        let expected_tokens = vec![
            (TypeLiteral(I8), 1, (1, 2)),
            (TypeLiteral(I16), 1, (4, 6)),
            (TypeLiteral(I32), 1, (8, 10)),
            (TypeLiteral(I64), 1, (12, 14)),
            (TypeLiteral(U8), 1, (16, 17)),
            (TypeLiteral(U16), 1, (19, 21)),
            (TypeLiteral(U32), 1, (23, 25)),
            (TypeLiteral(U64), 1, (27, 29)),
            (TypeLiteral(F32), 1, (31, 33)),
            (TypeLiteral(F64), 1, (35, 37)),
            (TypeLiteral(Bool), 1, (39, 42)),
            (TypeLiteral(Nil), 1, (44, 46)),
        ];

        let tokens = lexer
            .tokens
            .iter()
            .skip(1)
            .map(|t| (t.kind, t.span.start.0, (t.span.start.1, t.span.end.1)))
            .collect::<Vec<_>>();

        assert_eq!(lexer.tokens.len() - 1, expected_tokens.len());
        for (i, token) in tokens.iter().enumerate() {
            assert_eq!(token, &expected_tokens[i]);
        }
    }
}
