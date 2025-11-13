use crate::compiler::tokens::{Punctuation, Token, TokenKind};

use super::Lexer;

impl<'a> Lexer<'a> {
    fn push_punctuation(&mut self, punct: Punctuation, chars_consumed: usize) {
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
        self.tokens.push(Token::new(TokenKind::Punctuation(punct), span));
    }

    pub fn check_punctuation(&mut self) -> bool {
        use crate::compiler::tokens::Punctuation::*;

        const PATTERNS: &[(&str, Punctuation, usize)] = &[
            ("<<=", LeftLeftEq, 3),
            (">>=", RightRightEq, 3),
            ("<<", LeftLeft, 3),
            (">>", RightRight, 3),
            ("<=", LessThanOrEq, 3),
            (">=", GreaterThanOrEq, 3),
            ("<", LessThan, 3),
            (">", GreaterThan, 3),
            // 2-char ops (checked first)
            ("&&", AmpAmp, 2),
            ("!=", NotEq, 2),
            ("**", StarStar, 2),
            ("==", EqEq, 2),
            ("||", PipePipe, 2),
            ("=>", FatArrow, 2),
            ("->", Arrow, 2),
            // 1-char ops that require 2 chars (for precedence)
            ("&", Amp, 2),
            ("!", Bang, 2),
            ("*", Star, 2),
            ("=", Eq, 2),
            ("|", Pipe, 2),
            ("-", Minus, 2),
            // 1-char ops that only require 1 char
            (")", CloseParen, 1),
            ("(", OpenParen, 1),
            ("{", OpenBrace, 1),
            ("}", CloseBrace, 1),
            ("[", OpenBracket, 1),
            ("]", CloseBracket, 1),
            (",", Comma, 1),
            (".", Dot, 1),
            (":", Colon, 1),
            (";", Semicolon, 1),
            ("/", Slash, 1),
            ("%", Percent, 1),
            ("+", Plus, 1),
            ("@", At, 1),
            ("^", Caret, 2),
            ("~", Tilde, 1),
        ];

        for &(pattern, punctuation, min_chars) in PATTERNS {
            let pattern_len = pattern.len();

            if self.current_chars.len() >= min_chars {
                let matches = pattern
                    .bytes()
                    .enumerate()
                    .all(|(i, expected)| self.current_chars[i].1 as u8 == expected);

                if matches {
                    self.push_punctuation(punctuation, pattern_len);
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
