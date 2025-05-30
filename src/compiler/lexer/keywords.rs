use crate::compiler::tokens::Token;

use super::Lexer;
use crate::compiler::lexer::LexedChar;

impl Lexer {
    fn push_keyword(&mut self, keyword: crate::compiler::tokens::Keyword, chars_consumed: usize) {
        if chars_consumed == 0 || self.current_chars.is_empty() {
            return;
        }

        let start_pos = self.current_chars[0].0;
        let end_pos = if chars_consumed <= self.current_chars.len() {
            self.current_chars[chars_consumed - 1].0
        } else {
            self.current_chars.back().map(|c| c.0).unwrap_or(start_pos)
        };

        let span = self.create_span_from_positions(start_pos, end_pos);
        self.tokens
            .push(Token::new(crate::compiler::tokens::TokenKind::Keyword(keyword), span));
    }

    pub fn check_keywords(&mut self) -> bool {
        use crate::compiler::tokens::Keyword::*;

        macro_rules! check_keyword {
            ($str:expr, $keyword:expr) => {{
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
                        self.push_keyword($keyword, str_chars.len());
                        self.current_chars.drain(..str_chars.len());
                        return true;
                    }
                }
                false
            }};
        }

        check_keyword!("fn", Fn);
        check_keyword!("use", Use);
        check_keyword!("const", Const);
        check_keyword!("let", Let);
        check_keyword!("loop", Loop);
        check_keyword!("break", Break);
        check_keyword!("return", Return);
        check_keyword!("struct", Struct);
        check_keyword!("alloc", Alloc);
        check_keyword!("free", Free);
        check_keyword!("pub", Pub);
        check_keyword!("local", Local);
        check_keyword!("self", Self_);
        check_keyword!("as", As);
        check_keyword!("in", In);

        false
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use string_interner::{backend::BucketBackend, symbol::SymbolUsize, StringInterner};
    type Interner = StringInterner<BucketBackend<SymbolUsize>>;
    use crate::compiler::tokens::Keyword::*;
    use crate::compiler::tokens::TokenKind::*;

    #[test]
    fn test_lex_punctuation() {
        let mut interner = Interner::new();
        let file = interner.get_or_intern("");
        let contents = r#"fn use const let loop break return struct alloc free pub local self as in"#;
        let mut lexer = Lexer::new(contents, interner, file);

        let chars: Vec<char> = contents.chars().chain(std::iter::once('\0')).collect();
        lexer.tokenize(chars);

        let expected_tokens = vec![
            (Keyword(Fn), 1, (1, 2)),
            (Keyword(Use), 1, (4, 6)),
            (Keyword(Const), 1, (8, 12)),
            (Keyword(Let), 1, (14, 16)),
            (Keyword(Loop), 1, (18, 21)),
            (Keyword(Break), 1, (23, 27)),
            (Keyword(Return), 1, (29, 34)),
            (Keyword(Struct), 1, (36, 41)),
            (Keyword(Alloc), 1, (43, 47)),
            (Keyword(Free), 1, (49, 52)),
            (Keyword(Pub), 1, (54, 56)),
            (Keyword(Local), 1, (58, 62)),
            (Keyword(Self_), 1, (64, 67)),
            (Keyword(As), 1, (69, 70)),
            (Keyword(In), 1, (72, 73)),
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
