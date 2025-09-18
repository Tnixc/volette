use super::{
    Parser,
    error::ParserError,
    precedence::BindingPower,
};
use crate::compiler::tokens::{Keyword, Punctuation, TokenKind};
use generational_arena::Index;

impl Parser {
    pub fn parse_expr(&mut self) -> Result<Index, ParserError> {
        self.pratt_parse_expression(BindingPower::None)
    }

    /// the core pratt parsing loop.
    /// parses an expression whose components have at least `min_bp` binding power.
    pub fn pratt_parse_expression(&mut self, min_bp: BindingPower) -> Result<Index, ParserError> {
        // 1. handle nud (null denotation) for the current token (prefix context)
        let mut left_expr_idx: Index;
        let current_token = *self.current();

        match current_token.kind {
            TokenKind::IntLiteral(_) | TokenKind::FloatLiteral(_) | TokenKind::BoolLiteral(_) => {
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
            TokenKind::Keyword(Keyword::Loop) => {
                left_expr_idx = self.parse_loop_expr_nud(current_token)?;
            }
            TokenKind::Punctuation(Punctuation::OpenParen) => {
                left_expr_idx = self.parse_paren_expr_nud(current_token)?;
            }
            TokenKind::Punctuation(Punctuation::OpenBrace) => {
                left_expr_idx = self.parse_block_expr_nud(current_token)?;
            }
            _ => {
                return Err(ParserError::ExpressionExpected { token: current_token });
            }
        }

        // 2. handle led (left denotation) for subsequent tokens (infix/postfix context)
        loop {
            let next_token = *self.current();
            let (left_bp, is_right_associative) = match next_token.kind {
                TokenKind::Punctuation(Punctuation::Eq) => (BindingPower::Assignment, true),

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
                | TokenKind::Punctuation(Punctuation::PipePipe) => {
                    left_expr_idx = self.parse_binary_infix_op_led(next_token, left_expr_idx, left_bp, is_right_associative)?;
                }
                TokenKind::Punctuation(Punctuation::OpenParen) => {
                    left_expr_idx = self.parse_call_led(next_token, left_expr_idx)?;
                }

                _ => {
                    return Err(ParserError::InternalError(format!("Unhandled LED token: {:?}", next_token.kind)));
                }
            }
        }
        Ok(left_expr_idx)
    }
}