use generational_arena::Index;

use crate::compiler::error::Help;
use crate::compiler::tokens::Token;
use rootcause::prelude::*;

use super::{
    Parser,
    node::{ExprKind, Node, NodeKind},
};

impl<'a> Parser<'a> {
    pub fn parse_cast_led(&mut self, _as_token: Token, expr_idx: Index) -> Result<Index, Report> {
        let expr_node = self
            .node(&expr_idx)
            .ok_or_else(|| {
                crate::parse_err!(
                    "Node not found: cast expression node",
                    Some(self.current().span.to_display(self.interner))
                )
                .attach(Help("This is likely a parser bug - please report it".into()))
            })?
            .clone();

        let (target_type, type_span) = self.parse_type()?;

        let combined_span = expr_node.span.connect_new(&type_span);

        Ok(self.push(Node::new(
            NodeKind::Expr {
                kind: ExprKind::Cast {
                    expr: expr_idx,
                    target_type,
                },
                type_: None,
            },
            combined_span,
        )))
    }
}
