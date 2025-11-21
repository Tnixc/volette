use generational_arena::Index;

use crate::compiler::{
    error::Help,
    tokens::{Punctuation, Token, TokenKind},
};
use rootcause::prelude::*;

use super::{
    Parser,
    node::{ExprKind, Node, NodeKind, UnaryOpKind},
    precedence::BindingPower,
};

impl<'a> Parser<'a> {
    pub fn parse_unary_op_nud(&mut self, op_token: Token) -> Result<Index, Report> {
        let op_kind = match op_token.kind {
            TokenKind::Punctuation(Punctuation::Bang) => UnaryOpKind::Not,
            TokenKind::Punctuation(Punctuation::Minus) => UnaryOpKind::Neg,
            TokenKind::Punctuation(Punctuation::At) => UnaryOpKind::Deref,
            TokenKind::Punctuation(Punctuation::Amp) => UnaryOpKind::AddressOf,
            _ => {
                return Err(crate::parse_err!(
                    "Invalid unary operator: Token {:?} is not a unary prefix operator",
                    Some(op_token.span.to_display(self.interner)),
                    op_token.kind
                )
                .attach(Help("This construct is not valid in the current context".into())));
            }
        };

        self.advance(); // consume the operator

        // unary binding power
        let expr_idx = self.pratt_parse_expression(BindingPower::Unary)?;

        let expr_node = self
            .node(&expr_idx)
            .ok_or_else(|| {
                crate::parse_err!(
                    "Node not found: unary operand node",
                    Some(self.current().span.to_display(self.interner))
                )
                .attach(Help("This is likely a parser bug - please report it".into()))
            })?
            .clone();

        let combined_span = op_token.span.connect_new(&expr_node.span);

        Ok(self.push(Node::new(
            NodeKind::Expr {
                kind: ExprKind::UnaryOp {
                    op: op_kind,
                    expr: expr_idx,
                },
                type_: None,
            },
            combined_span,
        )))
    }
}
