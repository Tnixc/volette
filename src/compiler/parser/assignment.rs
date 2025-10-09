use generational_arena::Index;

use crate::compiler::tokens::Token;

use super::{
    Parser,
    error::ParserError,
    node::{ExprKind, Node, NodeKind},
    precedence::BindingPower,
};

impl<'a> Parser<'a> {
    pub fn parse_assignment_led(
        &mut self,
        _eq_token: Token,
        target_idx: Index,
        _is_right_assoc: bool, /* should be true */
    ) -> Result<Index, ParserError> {
        // L-value check (target must be assignable)
        let target_node_cloned = self
            .node(&target_idx)
            .ok_or_else(|| ParserError::NotFound {
                what: "assignment target node".to_string(),
                span: self.current().span.to_display(self.interner),
            })?
            .clone();
        match target_node_cloned.kind {
            NodeKind::Expr {
                kind: ExprKind::Identifier(_),
                ..
            } => { /* OK */ }
            // TODO: Add other valid L-values (field access, array index)
            _ => {
                return Err(ParserError::Invalid {
                    what: "assignment target".to_string(),
                    reason: "Only variables can be assigned to".to_string(),
                    span: target_node_cloned.span.to_display(&self.interner),
                });
            }
        }

        // for right-associativity, parse rhs with the same binding power.
        let value_idx = self.pratt_parse_expression(BindingPower::Assignment)?;

        let value_node_cloned = self.node(&value_idx).ok_or_else(|| ParserError::NotFound {
            what: "assignment value node".to_string(),
            span: self.current().span.to_display(self.interner),
        })?;
        let assignment_span = target_node_cloned.span.connect_new(&value_node_cloned.span);

        Ok(self.push(Node::new(
            NodeKind::Expr {
                kind: ExprKind::Assign {
                    target: target_idx,
                    value: value_idx,
                },
                type_: None,
            },
            assignment_span,
        )))
    }
}
