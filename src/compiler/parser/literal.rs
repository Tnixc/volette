use generational_arena::Index;

use crate::compiler::{
    error::Help,
    tokens::{Token, TokenKind},
};
use rootcause::prelude::*;

use super::{
    Parser,
    node::{ExprKind, Literal, Node, NodeKind},
};

impl<'a> Parser<'a> {
    pub fn parse_literal_nud(&mut self, literal_token: Token) -> Result<Index, Report> {
        let literal_kind = match literal_token.kind {
            TokenKind::IntLiteral(i) => Literal::Int(i),
            TokenKind::FloatLiteral(f) => Literal::Float(f),
            TokenKind::BoolLiteral(b) => Literal::Bool(b),
            _ => {
                return Err(crate::parse_err!(
                    "Invalid token in literal context: Expected literal, got {:?}",
                    Some(literal_token.span.to_display(self.interner)),
                    literal_token.kind
                )
                .attach(Help("This construct is not valid in the current context".into())));
            }
        };

        self.advance(); // consume the literal token

        Ok(self.push(Node::new(
            NodeKind::Expr {
                kind: ExprKind::Literal(literal_kind),
                type_: None,
            },
            literal_token.span,
        )))
    }
}
