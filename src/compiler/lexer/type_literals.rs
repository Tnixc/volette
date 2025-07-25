use crate::compiler::tokens::{Token, TokenKind};

use super::Lexer;
use crate::compiler::lexer::LexedChar;

impl Lexer {
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

        macro_rules! check_type {
            ($str:expr, $keyword:expr) => {{
                let str_chars: Vec<char> = $str.chars().collect();

                if self.current_chars.len() >= str_chars.len()
                    && self
                        .current_chars
                        .iter()
                        .take(str_chars.len())
                        .zip(str_chars.iter())
                        .all(|((_, char_from_input), &expected_char)| *char_from_input == expected_char)
                {
                    let has_valid_boundary = if self.current_chars.len() > str_chars.len() {
                        !self.current_chars[str_chars.len()].1.is_valid_ident_char()
                    } else {
                        true
                    };

                    if has_valid_boundary {
                        self.push_type_literal($keyword, str_chars.len());
                        self.current_chars.drain(..str_chars.len());
                        return true;
                    }
                }
                false
            }};
        }

        check_type!("i8", I8);
        check_type!("i16", I16);
        check_type!("i32", I32);
        check_type!("i64", I64);
        check_type!("u8", U8);
        check_type!("u16", U16);
        check_type!("u32", U32);
        check_type!("u64", U64);
        check_type!("f32", F32);
        check_type!("f64", F64);
        check_type!("usize", Usize);
        check_type!("isize", Isize);
        check_type!("bool", Bool);
        check_type!("Nil", Nil);

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
        let mut lexer = Lexer::new(interner, file);

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
