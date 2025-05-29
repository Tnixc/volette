use crate::compiler::tokens::{Token, TokenKind};

use super::Lexer;
use crate::compiler::lexer::IdentName;

impl Lexer {
    fn push_type_literal(&mut self, type_literal: crate::compiler::tokens::PrimitiveTypes, str_len: usize) {
        let span = self.create_span(self.cursor.col - str_len, self.cursor.col - 1);
        // I really no have no idea why this is -1 on the end, unlike the punctuation check, but it works
        // I hate off by one errors
        self.tokens.push(Token::new(TokenKind::TypeLiteral(type_literal), span));
    }

    pub fn check_type_literals(&mut self) -> bool {
        use crate::compiler::tokens::PrimitiveTypes::*;

        macro_rules! check_type {
            ($str:expr, $keyword:expr) => {{
                if self.current_str.len() > $str.len()
                    && self
                        .current_str
                        .chars()
                        .nth($str.len())
                        .is_some_and(|c| !c.is_valid_ident_char())
                    && self.current_str.starts_with($str)
                {
                    println!("{}", self.current_str);
                    println!("{}", $str);
                    println!("{}", "MATCH");
                    self.current_str.drain(0..=($str.len() - 1));
                    self.push_type_literal($keyword, $str.len());
                    return true;
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
        check_type!("bool", Bool);
        check_type!("None", None);

        false
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use string_interner::{backend::BucketBackend, symbol::SymbolUsize, StringInterner};
    type Interner = StringInterner<BucketBackend<SymbolUsize>>;
    use crate::compiler::tokens::PrimitiveTypes::*;
    use crate::compiler::tokens::TokenKind::*;

    #[test]
    fn test_lex_types() {
        let mut interner = Interner::new();
        let file = interner.get_or_intern("");
        let contents = r#"i8 i16 i32 i64 u8 u16 u32 u64 f32 f64 bool None"#;
        let mut lexer = Lexer::new(contents, interner, file);

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
            (TypeLiteral(None), 1, (44, 47)),
        ];

        assert_eq!(lexer.tokens.len(), expected_tokens.len());

        for (i, expected) in expected_tokens.iter().enumerate() {
            assert_eq!(
                lexer.tokens[i].kind, expected.0,
                "Token at index {} should be {:?}",
                i, expected
            );
            assert_eq!(
                lexer.tokens[i].span.line, expected.1,
                "Token at index {} should be on line {}",
                i, expected.1
            );
            assert_eq!(
                lexer.tokens[i].span.start, expected.2 .0,
                "Token at index {} should start at {}",
                i, expected.2 .0
            );
            assert_eq!(
                lexer.tokens[i].span.end, expected.2 .1,
                "Token at index {} should end at {}",
                i, expected.2 .1
            );
        }
    }
}
