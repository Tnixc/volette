use generational_arena::Index;

use crate::compiler::tokens::{Punctuation, Token, TokenKind};

use super::{
    Parser,
    error::ParserError,
    node::{BinOpKind, ExprKind, Node, NodeKind},
    precedence::BindingPower,
};

impl<'a> Parser<'a> {
    pub fn parse_binary_infix_op_led(
        &mut self,
        op_token: Token,
        left_idx: Index,
        op_bp: BindingPower,
        is_right_assoc: bool,
    ) -> Result<Index, ParserError> {
        let op_kind = match op_token.kind {
            TokenKind::Punctuation(Punctuation::Plus) => BinOpKind::Add,
            TokenKind::Punctuation(Punctuation::Minus) => BinOpKind::Sub,
            TokenKind::Punctuation(Punctuation::Star) => BinOpKind::Mul,
            TokenKind::Punctuation(Punctuation::Slash) => BinOpKind::Div,
            TokenKind::Punctuation(Punctuation::Percent) => BinOpKind::Mod,
            TokenKind::Punctuation(Punctuation::EqEq) => BinOpKind::Eq,
            TokenKind::Punctuation(Punctuation::NotEq) => BinOpKind::NotEq,
            TokenKind::Punctuation(Punctuation::LessThan) => BinOpKind::LessThan,
            TokenKind::Punctuation(Punctuation::LessThanOrEq) => BinOpKind::LessThanOrEq,
            TokenKind::Punctuation(Punctuation::GreaterThan) => BinOpKind::GreaterThan,
            TokenKind::Punctuation(Punctuation::GreaterThanOrEq) => BinOpKind::GreaterThanOrEq,
            TokenKind::Punctuation(Punctuation::AmpAmp) => BinOpKind::LogicalAnd,
            TokenKind::Punctuation(Punctuation::PipePipe) => BinOpKind::LogicalOr,
            TokenKind::Punctuation(Punctuation::Amp) => BinOpKind::BitwiseAnd,
            TokenKind::Punctuation(Punctuation::Pipe) => BinOpKind::BitwiseOr,
            TokenKind::Punctuation(Punctuation::Caret) => BinOpKind::BitwiseXor,
            TokenKind::Punctuation(Punctuation::LeftLeft) => BinOpKind::BitwiseShLeft,
            TokenKind::Punctuation(Punctuation::RightRight) => BinOpKind::BitwiseShRight,
            _ => {
                return Err(ParserError::Invalid {
                    what: "binary operator".to_string(),
                    reason: format!("Token {:?} is not a binary infix operator", op_token.kind),
                    span: op_token.span.to_display(self.interner),
                });
            }
        };

        let right_bp = if is_right_assoc { op_bp } else { op_bp + BindingPower::from(1) }; // Next level for left-assoc
        let right_idx = self.pratt_parse_expression(right_bp)?;

        let left_node_cloned = self
            .node(&left_idx)
            .ok_or_else(|| ParserError::NotFound {
                what: "left operand node".to_string(),
                span: self.current().span.to_display(self.interner),
            })?
            .clone();
        let right_node_cloned = self
            .node(&right_idx)
            .ok_or_else(|| ParserError::NotFound {
                what: "right operand node".to_string(),
                span: self.current().span.to_display(self.interner),
            })?
            .clone();
        let combined_span = left_node_cloned.span.connect_new(&right_node_cloned.span);

        Ok(self.push(Node::new(
            NodeKind::Expr {
                kind: ExprKind::BinOp {
                    left: left_idx,
                    right: right_idx,
                    op: op_kind,
                },
                type_: None,
            },
            combined_span,
        )))
    }
}
