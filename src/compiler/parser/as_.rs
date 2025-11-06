use generational_arena::Index;

use crate::compiler::tokens::Token;

use super::{
    Parser,
    error::ParserError,
    node::{ExprKind, Node, NodeKind},
};

impl<'a> Parser<'a> {
    pub fn parse_cast_led(&mut self, _as_token: Token, expr_idx: Index) -> Result<Index, ParserError> {
        let expr_node = self
            .node(&expr_idx)
            .ok_or_else(|| ParserError::NotFound {
                what: "cast expression node".to_string(),
                span: self.current().span.to_display(self.interner),
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
