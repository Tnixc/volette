use crate::compiler::tokens::{Punctuation, Token, TokenKind};

use super::Lexer;

impl<'a> Lexer<'a> {
    fn push_punctuation(&mut self, punct: Punctuation, chars_consumed: usize) {
        if chars_consumed == 0 || self.current_chars.is_empty() {
            return;
        }

        // skip consecutive semicolons, leave one
        if punct == Punctuation::Semicolon
            && let Some(last_token) = self.tokens.last()
            && last_token.kind == TokenKind::Punctuation(Punctuation::Semicolon)
        {
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

    const PATTERNS: &'static [(&'static str, Punctuation, usize)] = {
        use crate::compiler::tokens::Punctuation::*;
        &[
            ("<<=", LeftLeftEq, 3),
            (">>=", RightRightEq, 3),
            ("<<", LeftLeft, 3),
            (">>", RightRight, 3),
            ("<=", LessThanOrEq, 3),
            (">=", GreaterThanOrEq, 3),
            ("<", LessThan, 3),
            (">", GreaterThan, 3),
            ("**=", StarStarEq, 3),
            ("**", StarStar, 3),
            ("*", Star, 3),
            // 2-char ops (checked first)
            ("&&", AmpAmp, 2),
            ("&=", AmpEq, 2),
            ("!=", NotEq, 2),
            ("^=", CaretEq, 2),
            ("~=", TildeEq, 2),
            ("==", EqEq, 2),
            ("||", PipePipe, 2),
            ("|=", PipeEq, 2),
            ("=>", FatArrow, 2),
            ("->", Arrow, 2),
            // 1-char ops that require 2 chars (for precedence)
            ("&", Amp, 2),
            ("!", Bang, 2),
            ("=", Eq, 2),
            ("|", Pipe, 2),
            ("-", Minus, 2),
            ("^", Caret, 2),
            ("~", Tilde, 2),
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
        ]
    };

    pub fn check_punctuation(&mut self) -> bool {
        for &(pattern, punctuation, min_chars) in Self::PATTERNS {
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

    /// Check for single-char punctuation, ignoring min_chars requirement.
    /// Used when we know the next char won't form a multi-char operator (e.g., before a digit).
    pub fn check_single_char_punctuation(&mut self) -> bool {
        if self.current_chars.len() != 1 {
            return false;
        }

        for &(pattern, punctuation, _) in Self::PATTERNS {
            if pattern.len() == 1 && self.current_chars[0].1 as u8 == pattern.as_bytes()[0] {
                self.push_punctuation(punctuation, 1);
                self.current_chars.drain(..1);
                return true;
            }
        }

        false
    }
}
