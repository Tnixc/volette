use crate::compiler::tokens::{Token, TokenKind};

use super::{Lexer, LexerState};
use crate::compiler::error::Help;
use rootcause::prelude::*;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum NumberBase {
    Decimal,
    Hex,
    Binary,
    Octal,
}

impl<'a> Lexer<'a> {
    pub fn lex_number(&mut self, c: char, float: bool, base: NumberBase) -> bool {
        let current_string = self.current_chars.iter().map(|(_, ch)| *ch).collect::<String>();

        if current_string == "0" && (c == 'x' || c == 'b' || c == 'o') {
            self.current_chars.push_back(((self.cursor.line, self.cursor.col), c));
            self.state = LexerState::Number(
                float,
                match c {
                    'x' => NumberBase::Hex,
                    'b' => NumberBase::Binary,
                    'o' => NumberBase::Octal,
                    _ => return true,
                },
            );
            return true;
        }

        let is_valid_char = if c == '.' && !float {
            self.state = LexerState::Number(true, base);
            true
        } else {
            c == '_'
                || base == NumberBase::Octal && c.is_digit(8)
                || base == NumberBase::Binary && c.is_digit(2)
                || base == NumberBase::Hex && c.is_ascii_hexdigit()
                || base == NumberBase::Decimal && c.is_ascii_digit()
        };

        if is_valid_char {
            self.current_chars.push_back(((self.cursor.line, self.cursor.col), c));
            true
        } else {
            self.push_number(float, base);
            self.state = LexerState::Normal;
            false
        }
    }

    fn push_number(&mut self, float: bool, base: NumberBase) {
        if self.current_chars.is_empty() {
            return;
        }

        let start_pos = self.current_chars[0].0;
        let end_pos = self.current_chars.back().map(|(pos, _)| *pos).unwrap_or(start_pos);
        let span = self.create_span(start_pos.0, start_pos.1, end_pos.0, end_pos.1);

        if float {
            if base != NumberBase::Decimal {
                self.errors.push(
                    crate::lex_err!(
                        "Invalid float literal: floating-point literals must use decimal notation",
                        Some(span.to_display(&self.interner))
                    )
                    .attach(Help("Check the syntax of this token".into()))
                    .into_cloneable(),
                );
            } else if let Ok(n) = self.lex_float() {
                self.tokens.push(Token::new(TokenKind::FloatLiteral(n), span));
            }
        } else if let Ok(n) = self.lex_int(base) {
            self.tokens.push(Token::new(TokenKind::IntLiteral(n), span));
        }

        self.state = LexerState::Normal;
        self.current_chars.drain(..self.current_chars.len()).for_each(|_| {});
    }

    pub fn lex_float(&mut self) -> Result<f64, Report> {
        let current_string = self.current_chars.iter().map(|(_, ch)| *ch).collect::<String>();
        let filtered_str: String = current_string.chars().filter(|&c| c != '_').collect();

        filtered_str.parse::<f64>().map_err(|_| {
            let start_pos = self
                .current_chars
                .front()
                .map(|(pos, _)| *pos)
                .unwrap_or((self.cursor.line, self.cursor.col));
            let end_pos = self.current_chars.back().map(|(pos, _)| *pos).unwrap_or(start_pos);
            let span = self.create_span(start_pos.0, start_pos.1, end_pos.0, end_pos.1);

            let msg = format!("Invalid float '{}': cannot be parsed as a floating-point number", filtered_str);
            let display_span = span.to_display(&self.interner);

            self.errors.push(
                crate::lex_err!(msg.clone(), Some(display_span.clone()))
                    .attach(Help("Check the syntax of this token".into()))
                    .into_cloneable(),
            );

            crate::lex_err!(msg, Some(display_span)).attach(Help("Check the syntax of this token".into()))
        })
    }

    pub fn lex_int(&mut self, base: NumberBase) -> Result<i64, Report> {
        let radix = match base {
            NumberBase::Decimal => 10,
            NumberBase::Hex => 16,
            NumberBase::Binary => 2,
            NumberBase::Octal => 8,
        };

        let current_string = self.current_chars.iter().map(|(_, ch)| *ch).collect::<String>();
        let filtered_str: String = current_string.chars().filter(|&c| c != '_').collect();

        let parse_str = if base == NumberBase::Decimal {
            filtered_str.clone()
        } else {
            filtered_str.get(2..).unwrap_or("").to_string()
        };

        let result = if base == NumberBase::Decimal {
            parse_str.parse::<i64>()
        } else {
            i64::from_str_radix(&parse_str, radix)
        };

        result.map_err(|_| {
            let start_pos = self
                .current_chars
                .front()
                .map(|(pos, _)| *pos)
                .unwrap_or((self.cursor.line, self.cursor.col));
            let end_pos = self.current_chars.back().map(|(pos, _)| *pos).unwrap_or(start_pos);
            let span = self.create_span(start_pos.0, start_pos.1, end_pos.0, end_pos.1);

            let msg = format!("Invalid integer '{}': cannot be parsed as an integer", filtered_str);
            let display_span = span.to_display(&self.interner);

            self.errors.push(
                crate::lex_err!(msg.clone(), Some(display_span.clone()))
                    .attach(Help("Check the syntax of this token".into()))
                    .into_cloneable(),
            );

            crate::lex_err!(msg, Some(display_span)).attach(Help("Check the syntax of this token".into()))
        })
    }
}
