use generational_arena::Index;

use crate::compiler::tokens::{Punctuation, Token, TokenKind};

use super::{
    Parser,
    error::ParserError,
    node::{ExprKind, Node, NodeKind, UnaryOpKind},
    precedence::BindingPower,
};

impl<'a> Parser<'a> {
    pub fn parse_unary_op_nud(&mut self, op_token: Token) -> Result<Index, ParserError> {
        let op_kind = match op_token.kind {
            TokenKind::Punctuation(Punctuation::Bang) => UnaryOpKind::Not,
            TokenKind::Punctuation(Punctuation::Minus) => UnaryOpKind::Neg,
            TokenKind::Punctuation(Punctuation::At) => UnaryOpKind::Deref,
            TokenKind::Punctuation(Punctuation::Amp) => UnaryOpKind::AddressOf,
            _ => {
                return Err(ParserError::Invalid {
                    what: "unary operator".to_string(),
                    reason: format!("Token {:?} is not a unary prefix operator", op_token.kind),
                    span: op_token.span.to_display(self.interner),
                });
            }
        };

        self.advance(); // consume the operator

        // unary binding power
        let expr_idx = self.pratt_parse_expression(BindingPower::Unary)?;

        let expr_node = self
            .node(&expr_idx)
            .ok_or_else(|| ParserError::NotFound {
                what: "unary operand node".to_string(),
                span: self.current().span.to_display(self.interner),
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
