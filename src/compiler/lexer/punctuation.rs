use crate::compiler::tokens::Token;

use super::Lexer;

impl<'a> Lexer<'a> {
    fn push_punctuation(&mut self, punct: crate::compiler::tokens::Punctuation, chars_consumed: usize) {
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
        self.tokens
            .push(Token::new(crate::compiler::tokens::TokenKind::Punctuation(punct), span));
    }

    pub fn check_punctuation(&mut self) -> bool {
        use crate::compiler::tokens::Punctuation::*;

        macro_rules! check_punct {
            ($str:expr, $punct:expr, $chars_consumed:expr) => {{
                let str_chars: Vec<char> = $str.chars().collect();

                if self.current_chars.len() >= $chars_consumed {
                    let matches = self
                        .current_chars
                        .iter()
                        .take(str_chars.len())
                        .zip(str_chars.iter())
                        .all(|((_, char_from_input), &expected_char)| *char_from_input == expected_char);

                    if matches {
                        self.push_punctuation($punct, str_chars.len());
                        self.current_chars.drain(..str_chars.len());
                        return true;
                    }
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
        check_punct!("=>", FatArrow, 2);

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
    use string_interner::{StringInterner, backend::BucketBackend, symbol::SymbolUsize};
    type Interner = StringInterner<BucketBackend<SymbolUsize>>;
    use crate::compiler::tokens::Punctuation::*;
    use crate::compiler::tokens::TokenKind::*;

    #[test]
    fn test_lex_punctuation() {
        let mut interner = Interner::new();
        let file = interner.get_or_intern("");
        let contents = r#"&&  !=   ** ==  <=
>=   ||    &    !
*    =  <>    |    ) (
    {   }  [  ]   , .  :
;   /   %   +   -
=>"#;
        let mut lexer = Lexer::new(&mut interner, file);

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
            (Punctuation(Amp), 2, (12, 12)),
            (Punctuation(Bang), 2, (17, 17)),
            (Punctuation(Star), 3, (1, 1)),
            (Punctuation(Eq), 3, (6, 6)),
            (Punctuation(LessThan), 3, (9, 9)),
            (Punctuation(GreaterThan), 3, (10, 10)),
            (Punctuation(Pipe), 3, (15, 15)),
            (Punctuation(CloseParen), 3, (20, 20)),
            (Punctuation(OpenParen), 3, (22, 22)),
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
            (Punctuation(FatArrow), 6, (1, 2)),
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
