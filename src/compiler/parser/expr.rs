use super::{Parser, precedence::BindingPower};
use crate::compiler::{
    error::Help,
    tokens::{Keyword, Punctuation, TokenKind},
};
use generational_arena::Index;
use rootcause::prelude::*;

impl<'a> Parser<'a> {
    pub fn parse_expr(&mut self) -> Result<Index, Report> {
        self.pratt_parse_expression(BindingPower::None)
    }

    /// the core pratt parsing loop.
    /// parses an expression whose components have at least `min_bp` binding power.
    pub fn pratt_parse_expression(&mut self, min_bp: BindingPower) -> Result<Index, Report> {
        // while self.current().kind == TokenKind::Punctuation(Punctuation::Semicolon) {
        //     self.advance();
        // }

        // 1. handle nud (null denotation) for the current token (prefix context)
        let mut left_expr_idx: Index;
        let current_token = *self.current();

        match current_token.kind {
            TokenKind::IntLiteral(_) | TokenKind::FloatLiteral(_) | TokenKind::BoolLiteral(_) | TokenKind::NilLiteral => {
                left_expr_idx = self.parse_literal_nud(current_token)?;
            }
            TokenKind::Identifier(_) => {
                left_expr_idx = self.parse_identifier_nud(current_token)?;
            }
            TokenKind::Keyword(Keyword::Let) => {
                left_expr_idx = self.parse_let_expr_nud(current_token)?;
            }
            TokenKind::Keyword(Keyword::Return) => {
                left_expr_idx = self.parse_return_expr_nud(current_token)?;
            }
            TokenKind::Keyword(Keyword::Break) => {
                left_expr_idx = self.parse_break_expr_nud(current_token)?;
            }
            TokenKind::Keyword(Keyword::If) => {
                left_expr_idx = self.parse_if_expr_nud(current_token)?;
            }
            TokenKind::Keyword(Keyword::While) => {
                left_expr_idx = self.parse_while(current_token)?;
            }
            TokenKind::Punctuation(Punctuation::OpenParen) => {
                left_expr_idx = self.parse_paren_expr_nud(current_token)?;
            }
            TokenKind::Punctuation(Punctuation::OpenBrace) => {
                left_expr_idx = self.parse_block_expr_nud(current_token)?;
            }
            TokenKind::Punctuation(Punctuation::Bang)
            | TokenKind::Punctuation(Punctuation::Minus)
            | TokenKind::Punctuation(Punctuation::At)
            | TokenKind::Punctuation(Punctuation::Amp) => {
                left_expr_idx = self.parse_unary_op_nud(current_token)?;
            }
            _ => {
                return Err(crate::parse_err!(
                    "Expected expression, got {:?}",
                    Some(current_token.span.to_display(self.interner)),
                    current_token.kind
                )
                .attach(Help("Check the syntax - the parser expected something different here".into())));
            }
        }

        // 2. handle led (left denotation) for subsequent tokens (infix/postfix context)
        loop {
            let next_token = *self.current();
            let (left_bp, is_right_associative) = match next_token.kind {
                TokenKind::Punctuation(Punctuation::Eq)
                | TokenKind::Punctuation(Punctuation::PlusEq)
                | TokenKind::Punctuation(Punctuation::MinusEq)
                | TokenKind::Punctuation(Punctuation::StarEq)
                | TokenKind::Punctuation(Punctuation::SlashEq)
                | TokenKind::Punctuation(Punctuation::PercentEq)
                | TokenKind::Punctuation(Punctuation::AmpEq)
                | TokenKind::Punctuation(Punctuation::PipeEq)
                | TokenKind::Punctuation(Punctuation::CaretEq)
                | TokenKind::Punctuation(Punctuation::LeftLeftEq)
                | TokenKind::Punctuation(Punctuation::RightRightEq) => (BindingPower::Assignment, true),

                TokenKind::Punctuation(Punctuation::Plus) | TokenKind::Punctuation(Punctuation::Minus) => (BindingPower::Term, false),

                TokenKind::Punctuation(Punctuation::Star)
                | TokenKind::Punctuation(Punctuation::Slash)
                | TokenKind::Punctuation(Punctuation::Percent) => (BindingPower::Factor, false),

                TokenKind::Punctuation(Punctuation::EqEq) | TokenKind::Punctuation(Punctuation::NotEq) => (BindingPower::Equality, false),

                TokenKind::Punctuation(Punctuation::LessThan)
                | TokenKind::Punctuation(Punctuation::LessThanOrEq)
                | TokenKind::Punctuation(Punctuation::GreaterThan)
                | TokenKind::Punctuation(Punctuation::GreaterThanOrEq) => (BindingPower::Comparison, false),

                TokenKind::Punctuation(Punctuation::AmpAmp) => (BindingPower::LogicalAnd, false),
                TokenKind::Punctuation(Punctuation::PipePipe) => (BindingPower::LogicalOr, false),

                TokenKind::Punctuation(Punctuation::Amp) => (BindingPower::BitwiseAnd, false),
                TokenKind::Punctuation(Punctuation::Pipe) => (BindingPower::BitwiseOr, false),
                TokenKind::Punctuation(Punctuation::Caret) => (BindingPower::BitwiseXor, false),

                TokenKind::Punctuation(Punctuation::LeftLeft) => (BindingPower::BitwiseAnd, false),
                TokenKind::Punctuation(Punctuation::RightRight) => (BindingPower::BitwiseAnd, false),

                TokenKind::Keyword(Keyword::As) => (BindingPower::Cast, false),

                TokenKind::Punctuation(Punctuation::OpenParen) => (BindingPower::Call, false),

                _ => (BindingPower::None, false),
            };

            if left_bp < min_bp || left_bp == BindingPower::None {
                break;
            }

            self.advance();

            match next_token.kind {
                TokenKind::Punctuation(Punctuation::Eq) => {
                    left_expr_idx = self.parse_assignment_led(next_token, left_expr_idx, is_right_associative)?;
                }
                TokenKind::Punctuation(Punctuation::PlusEq)
                | TokenKind::Punctuation(Punctuation::MinusEq)
                | TokenKind::Punctuation(Punctuation::StarEq)
                | TokenKind::Punctuation(Punctuation::SlashEq)
                | TokenKind::Punctuation(Punctuation::PercentEq)
                | TokenKind::Punctuation(Punctuation::AmpEq)
                | TokenKind::Punctuation(Punctuation::PipeEq)
                | TokenKind::Punctuation(Punctuation::CaretEq)
                | TokenKind::Punctuation(Punctuation::LeftLeftEq)
                | TokenKind::Punctuation(Punctuation::RightRightEq) => {
                    left_expr_idx = self.parse_compound_assignment_led(next_token, left_expr_idx)?;
                }
                TokenKind::Punctuation(Punctuation::Plus)
                | TokenKind::Punctuation(Punctuation::Minus)
                | TokenKind::Punctuation(Punctuation::Star)
                | TokenKind::Punctuation(Punctuation::Slash)
                | TokenKind::Punctuation(Punctuation::Percent)
                | TokenKind::Punctuation(Punctuation::EqEq)
                | TokenKind::Punctuation(Punctuation::NotEq)
                | TokenKind::Punctuation(Punctuation::LessThan)
                | TokenKind::Punctuation(Punctuation::LessThanOrEq)
                | TokenKind::Punctuation(Punctuation::GreaterThan)
                | TokenKind::Punctuation(Punctuation::GreaterThanOrEq)
                | TokenKind::Punctuation(Punctuation::AmpAmp)
                | TokenKind::Punctuation(Punctuation::PipePipe)
                | TokenKind::Punctuation(Punctuation::Amp)
                | TokenKind::Punctuation(Punctuation::Pipe)
                | TokenKind::Punctuation(Punctuation::Caret)
                | TokenKind::Punctuation(Punctuation::LeftLeft)
                | TokenKind::Punctuation(Punctuation::RightRight) => {
                    left_expr_idx = self.parse_binary_infix_op_led(next_token, left_expr_idx, left_bp, is_right_associative)?;
                }
                TokenKind::Keyword(Keyword::As) => {
                    left_expr_idx = self.parse_cast_led(next_token, left_expr_idx)?;
                }
                TokenKind::Punctuation(Punctuation::OpenParen) => {
                    left_expr_idx = self.parse_call_led(next_token, left_expr_idx)?;
                }

                _ => {
                    return Err(crate::parse_err!(
                        "Node not found: handler for infix/postfix token: {:?}",
                        Some(next_token.span.to_display(self.interner)),
                        next_token.kind
                    )
                    .attach(Help("This is likely a parser bug - please report it".into())));
                }
            }
        }
        Ok(left_expr_idx)
    }
}
