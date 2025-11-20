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
