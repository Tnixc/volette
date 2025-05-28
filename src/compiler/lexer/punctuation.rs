use crate::compiler::tokens::Token;

use super::Lexer;

impl Lexer {
    fn push_punctuation(&mut self, punct: crate::compiler::tokens::Punctuation) {
        self.tokens.push(Token::new(
            crate::compiler::tokens::TokenKind::Punctuation(punct),
            self.span.clone(),
        ));
    }

    fn push_two_char_punct(&mut self, punct: crate::compiler::tokens::Punctuation) {
        self.span.end += 1;
        self.push_punctuation(punct);
    }

    pub fn lex_punctuation(&mut self, c: char, window: &[char]) -> bool {
        use crate::compiler::tokens::Punctuation::*;

        let start = self.span.end;
        self.span.start = start;

        macro_rules! single {
            ($punct:expr) => {{
                self.push_punctuation($punct);
                false
            }};
        }

        /// for 'x', and 'xy',
        /// 'x' => double_perchance!('y', XY, X)
        macro_rules! double_perchance {
            ($next:literal, $double:expr, $single:expr) => {
                if window.get(1) == Some(&$next) {
                    self.push_two_char_punct($double);
                    true
                } else {
                    self.push_punctuation($single);
                    false
                }
            };
        }

        match c {
            '(' => single!(OpenParen),
            ')' => single!(CloseParen),
            '{' => single!(OpenBrace),
            '}' => single!(CloseBrace),
            '[' => single!(OpenBracket),
            ']' => single!(CloseBracket),
            ',' => single!(Comma),
            '.' => single!(Dot),
            ':' => single!(Colon),
            ';' => single!(Semicolon),
            '/' => single!(Slash),
            '-' => single!(Minus),
            '+' => single!(Plus),
            '%' => single!(Percent),

            '!' => double_perchance!('=', NotEq, Bang),
            '=' => double_perchance!('=', EqEq, Eq),
            '>' => double_perchance!('=', GreaterThanOrEq, GreaterThan),
            '<' => double_perchance!('=', LessThanOrEq, LessThan),
            '&' => double_perchance!('&', AmpAmp, Amp),
            '*' => double_perchance!('*', StarStar, Star),
            '|' => double_perchance!('|', PipePipe, Pipe),
            _ => false,
        }
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
        let contents = "( [ + >= > !! * ** <= < - = != == & ||| ] ) {}";
        let mut lexer = Lexer::new(contents, file);

        let chars: Vec<char> = contents.chars().chain(std::iter::once('\0')).collect();
        lexer.tokenize(chars);
        assert_eq!(lexer.tokens.len(), 22);
        assert_eq!(lexer.tokens[0].kind, Punctuation(OpenParen));
        assert_eq!(lexer.tokens[1].kind, Punctuation(OpenBracket));
        assert_eq!(lexer.tokens[2].kind, Punctuation(Plus));
        assert_eq!(lexer.tokens[3].kind, Punctuation(GreaterThanOrEq));
        assert_eq!(lexer.tokens[4].kind, Punctuation(GreaterThan));
        assert_eq!(lexer.tokens[5].kind, Punctuation(Bang));
        assert_eq!(lexer.tokens[6].kind, Punctuation(Bang));
        assert_eq!(lexer.tokens[7].kind, Punctuation(Star));
        assert_eq!(lexer.tokens[8].kind, Punctuation(StarStar));
        assert_eq!(lexer.tokens[9].kind, Punctuation(LessThanOrEq));
        assert_eq!(lexer.tokens[10].kind, Punctuation(LessThan));
        assert_eq!(lexer.tokens[11].kind, Punctuation(Minus));
        assert_eq!(lexer.tokens[12].kind, Punctuation(Eq));
        assert_eq!(lexer.tokens[13].kind, Punctuation(NotEq));
        assert_eq!(lexer.tokens[14].kind, Punctuation(EqEq));
        assert_eq!(lexer.tokens[15].kind, Punctuation(Amp));
        assert_eq!(lexer.tokens[16].kind, Punctuation(PipePipe));
        assert_eq!(lexer.tokens[17].kind, Punctuation(Pipe));
        assert_eq!(lexer.tokens[18].kind, Punctuation(CloseBracket));
        assert_eq!(lexer.tokens[19].kind, Punctuation(CloseParen));
        assert_eq!(lexer.tokens[20].kind, Punctuation(OpenBrace));
        assert_eq!(lexer.tokens[21].kind, Punctuation(CloseBrace));
    }
}
