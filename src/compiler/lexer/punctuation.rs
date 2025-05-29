use crate::compiler::tokens::Token;

use super::Lexer;

impl Lexer {
    fn push_punctuation(&mut self, punct: crate::compiler::tokens::Punctuation, str_len: usize) {
        let span = self.create_span(self.cursor.col - str_len + 1, self.cursor.col);
        self.tokens
            .push(Token::new(crate::compiler::tokens::TokenKind::Punctuation(punct), span));
    }

    pub fn check_punctuation(&mut self) -> bool {
        use crate::compiler::tokens::Punctuation::*;

        macro_rules! check_punct {
            ($str:expr, $punct:expr, $min_check_len:expr) => {{
                let len = $min_check_len;
                if self.current_str.starts_with($str) && self.current_str.len() >= len {
                    self.current_str.drain(0..=($str.len() - 1));
                    self.push_punctuation($punct, $str.len());
                    return true;
                }
                false
            }};
        }

        check_punct!("&&", AmpAmp, 2);
        check_punct!("!=", NotEq, 2);
        check_punct!("**", StarStar, 2);
        check_punct!("==", EqEq, 2);
        check_punct!("<=", LessThanOrEq, 2);
        check_punct!(">=", GreaterThanOrEq, 2);
        check_punct!("||", PipePipe, 2);

        check_punct!("&", Amp, 2);
        check_punct!("!", Bang, 2);
        check_punct!("*", Star, 2);
        check_punct!("=", Eq, 2);
        check_punct!("<", LessThan, 2);
        check_punct!(">", GreaterThan, 2);
        check_punct!("|", Pipe, 2);

        check_punct!(")", CloseParen, 1);
        check_punct!("(", OpenParen, 1);
        check_punct!("{", OpenBrace, 1);
        check_punct!("}", CloseBrace, 1);
        check_punct!("[", OpenBracket, 1);
        check_punct!("]", CloseBracket, 1);
        check_punct!(",", Comma, 1);
        check_punct!(".", Dot, 1);
        check_punct!(":", Colon, 1);
        check_punct!(";", Semicolon, 1);
        check_punct!("/", Slash, 1);
        check_punct!("%", Percent, 1);
        check_punct!("+", Plus, 1);
        check_punct!("-", Minus, 1);

        false
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use string_interner::{backend::BucketBackend, symbol::SymbolUsize, StringInterner};
    type Interner = StringInterner<BucketBackend<SymbolUsize>>;
    use crate::compiler::tokens::Punctuation::*;
    use crate::compiler::tokens::TokenKind::*;

    #[test]
    fn test_lex_punctuation() {
        let mut interner = Interner::new();
        let file = interner.get_or_intern("");
        let contents = r#"&&  !=   ** ==  <=
>=   ||    &    !
*    =  <       >    |    ) (
    {   }  [  ]   , .  :
;   /   %   +   -"#;
        let mut lexer = Lexer::new(contents, interner, file);

        let chars: Vec<char> = contents.chars().chain(std::iter::once('\0')).collect();
        lexer.tokenize(chars);

        let expected_tokens = vec![
            (Punctuation(AmpAmp), 1, (1, 2)),
            (Punctuation(NotEq), 1, (5, 6)),
            (Punctuation(StarStar), 1, (10, 11)),
            (Punctuation(EqEq), 1, (13, 14)),
            (Punctuation(LessThanOrEq), 1, (17, 18)),
            (Punctuation(GreaterThanOrEq), 2, (1, 2)),
            (Punctuation(PipePipe), 2, (6, 7)),
            (Punctuation(Amp), 2, (13, 13)),
            (Punctuation(Bang), 3, (1, 1)),
            (Punctuation(Star), 3, (2, 2)),
            (Punctuation(Eq), 3, (7, 7)),
            (Punctuation(LessThan), 3, (10, 10)),
            (Punctuation(GreaterThan), 3, (18, 18)),
            (Punctuation(Pipe), 3, (23, 23)),
            (Punctuation(CloseParen), 3, (27, 27)),
            (Punctuation(OpenParen), 3, (29, 29)),
            (Punctuation(OpenBrace), 4, (5, 5)),
            (Punctuation(CloseBrace), 4, (9, 9)),
            (Punctuation(OpenBracket), 4, (12, 12)),
            (Punctuation(CloseBracket), 4, (15, 15)),
            (Punctuation(Comma), 4, (19, 19)),
            (Punctuation(Dot), 4, (21, 21)),
            (Punctuation(Colon), 4, (24, 24)),
            (Punctuation(Semicolon), 5, (1, 1)),
            (Punctuation(Slash), 5, (5, 5)),
            (Punctuation(Percent), 5, (9, 9)),
            (Punctuation(Plus), 5, (13, 13)),
            (Punctuation(Minus), 5, (17, 17)),
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
