use crate::compiler::tokens::{Punctuation, Token, TokenKind};

use super::Lexer;

impl Lexer {
    pub fn lex_punctuation(&mut self, c: char, window: &[char]) -> bool {
        use crate::compiler::tokens::Punctuation::*;
        use crate::compiler::tokens::TokenKind::*;

        let start = self.span.end;
        self.span.start = start;
        let skip_next = match c {
            '(' => {
                self.tokens.push(Token::new(Punctuation(OpenParen), self.span.clone()));
                false
            }
            ')' => {
                self.tokens.push(Token::new(Punctuation(CloseParen), self.span.clone()));
                false
            }
            '{' => {
                self.tokens.push(Token::new(Punctuation(OpenBrace), self.span.clone()));
                false
            }
            '}' => {
                self.tokens.push(Token::new(Punctuation(CloseBrace), self.span.clone()));
                false
            }
            '[' => {
                self.tokens
                    .push(Token::new(Punctuation(OpenBracket), self.span.clone()));
                false
            }
            ']' => {
                self.tokens
                    .push(Token::new(Punctuation(CloseBracket), self.span.clone()));
                false
            }
            ',' => {
                self.tokens.push(Token::new(Punctuation(Comma), self.span.clone()));
                false
            }
            '.' => {
                self.tokens.push(Token::new(Punctuation(Dot), self.span.clone()));
                false
            }
            ':' => {
                self.tokens.push(Token::new(Punctuation(Colon), self.span.clone()));
                false
            }
            ';' => {
                self.tokens.push(Token::new(Punctuation(Semicolon), self.span.clone()));
                false
            }
            '!' => {
                if window.get(1) == Some(&'=') {
                    self.span.end += 1;
                    self.tokens.push(Token::new(Punctuation(NotEq), self.span.clone()));
                    true
                } else {
                    self.tokens.push(Token::new(Punctuation(Bang), self.span.clone()));
                    false
                }
            }
            '=' => {
                if window.get(1) == Some(&'=') {
                    self.span.end += 1;
                    self.tokens.push(Token::new(Punctuation(EqEq), self.span.clone()));
                    true
                } else {
                    self.tokens.push(Token::new(Punctuation(Eq), self.span.clone()));
                    false
                }
            }
            '>' => {
                if window.get(1) == Some(&'=') {
                    self.span.end += 1;
                    self.tokens
                        .push(Token::new(Punctuation(GreaterThanOrEq), self.span.clone()));
                    true
                } else {
                    self.tokens
                        .push(Token::new(Punctuation(GreaterThan), self.span.clone()));
                    false
                }
            }
            '<' => {
                if window.get(1) == Some(&'=') {
                    self.span.end += 1;
                    self.tokens
                        .push(Token::new(Punctuation(LessThanOrEq), self.span.clone()));
                    true
                } else {
                    self.tokens.push(Token::new(Punctuation(LessThan), self.span.clone()));
                    false
                }
            }
            '&' => {
                if window.get(1) == Some(&'&') {
                    self.span.end += 1;
                    self.tokens.push(Token::new(Punctuation(And), self.span.clone()));
                    true
                } else {
                    self.tokens.push(Token::new(Punctuation(Ampersand), self.span.clone()));
                    false
                }
            }
            '/' => {
                self.tokens.push(Token::new(Punctuation(Slash), self.span.clone()));
                false
            }
            '*' => {
                if window.get(1) == Some(&'*') {
                    self.span.end += 1;
                    self.tokens.push(Token::new(Punctuation(StarStar), self.span.clone()));
                    true
                } else {
                    self.tokens.push(Token::new(Punctuation(Star), self.span.clone()));
                    false
                }
            }
            '-' => {
                self.tokens.push(Token::new(Punctuation(Minus), self.span.clone()));
                false
            }
            '+' => {
                self.tokens.push(Token::new(Punctuation(Plus), self.span.clone()));
                false
            }
            '%' => {
                self.tokens.push(Token::new(Punctuation(Percent), self.span.clone()));
                false
            }
            '|' => {
                if window.get(1) == Some(&'|') {
                    self.span.end += 1;
                    self.tokens.push(Token::new(Punctuation(PipePipe), self.span.clone()));
                    true
                } else {
                    false
                }
            }
            _ => false,
        };

        skip_next
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
        let contents = "( [ + >= > !! * ** <= < - = != == & || ] ) {}";
        let mut lexer = Lexer::new(contents, file);

        let chars: Vec<char> = contents.chars().chain(std::iter::once('\0')).collect();
        lexer.tokenize(chars);
        assert_eq!(lexer.tokens.len(), 21);
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
        assert_eq!(lexer.tokens[15].kind, Punctuation(Ampersand));
        assert_eq!(lexer.tokens[16].kind, Punctuation(PipePipe));
        assert_eq!(lexer.tokens[17].kind, Punctuation(CloseBracket));
        assert_eq!(lexer.tokens[18].kind, Punctuation(CloseParen));
        assert_eq!(lexer.tokens[19].kind, Punctuation(OpenBrace));
        assert_eq!(lexer.tokens[20].kind, Punctuation(CloseBrace));
    }
}
