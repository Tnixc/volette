use generational_arena::Index;

use crate::compiler::{
    error::Help,
    tokens::{Punctuation, Token, TokenKind},
};
use rootcause::prelude::*;

use super::{
    Parser,
    node::{BinOpKind, ExprKind, Node, NodeKind},
    precedence::BindingPower,
};

impl<'a> Parser<'a> {
    pub fn parse_assignment_led(
        &mut self,
        _eq_token: Token,
        target_idx: Index,
        _is_right_assoc: bool, /* should be true */
    ) -> Result<Index, Report> {
        // L-value check (target must be assignable)
        let target_node_cloned = self
            .node(&target_idx)
            .ok_or_else(|| {
                crate::parse_err!(
                    "Node not found: assignment target node",
                    Some(self.current().span.to_display(self.interner))
                )
                .attach(Help("This is likely a parser bug - please report it".into()))
            })?
            .clone();
        match target_node_cloned.kind {
            NodeKind::Expr {
                kind: ExprKind::Identifier(_),
                ..
            } => { /* OK */ }
            // TODO: Add other valid L-values (field access, array index)
            _ => {
                return Err(crate::parse_err!(
                    "Invalid assignment target: Only variables can be assigned to",
                    Some(target_node_cloned.span.to_display(&self.interner))
                )
                .attach(Help("This construct is not valid in the current context".into())));
            }
        }

        // for right-associativity, parse rhs with the same binding power.
        let value_idx = self.pratt_parse_expression(BindingPower::Assignment)?;

        let value_node_cloned = self.node(&value_idx).ok_or_else(|| {
            crate::parse_err!(
                "Node not found: assignment value node",
                Some(self.current().span.to_display(self.interner))
            )
            .attach(Help("This is likely a parser bug - please report it".into()))
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

    pub fn parse_compound_assignment_led(&mut self, op_token: Token, target_idx: Index) -> Result<Index, Report> {
        // Lvalue check (target must be assignable)
        let target_node_cloned = self
            .node(&target_idx)
            .ok_or_else(|| {
                crate::parse_err!(
                    "Node not found: assignment target node",
                    Some(self.current().span.to_display(self.interner))
                )
                .attach(Help("This is likely a parser bug - please report it".into()))
            })?
            .clone();
        match target_node_cloned.kind {
            NodeKind::Expr {
                kind: ExprKind::Identifier(_),
                ..
            } => { /* OK */ }
            _ => {
                return Err(crate::parse_err!(
                    "Invalid assignment target: Only variables can be assigned to",
                    Some(target_node_cloned.span.to_display(&self.interner))
                )
                .attach(Help("This construct is not valid in the current context".into())));
            }
        }

        let op = match op_token.kind {
            TokenKind::Punctuation(Punctuation::PlusEq) => BinOpKind::Add,
            TokenKind::Punctuation(Punctuation::MinusEq) => BinOpKind::Sub,
            TokenKind::Punctuation(Punctuation::StarEq) => BinOpKind::Mul,
            TokenKind::Punctuation(Punctuation::SlashEq) => BinOpKind::Div,
            TokenKind::Punctuation(Punctuation::PercentEq) => BinOpKind::Mod,
            TokenKind::Punctuation(Punctuation::AmpEq) => BinOpKind::BitwiseAnd,
            TokenKind::Punctuation(Punctuation::PipeEq) => BinOpKind::BitwiseOr,
            TokenKind::Punctuation(Punctuation::CaretEq) => BinOpKind::BitwiseXor,
            TokenKind::Punctuation(Punctuation::LeftLeftEq) => BinOpKind::BitwiseShLeft,
            TokenKind::Punctuation(Punctuation::RightRightEq) => BinOpKind::BitwiseShRight,
            _ => {
                return Err(crate::parse_err!(
                    "Invalid compound assignment operator: Unknown operator {:?}",
                    Some(op_token.span.to_display(&self.interner)),
                    op_token.kind
                )
                .attach(Help("This construct is not valid in the current context".into())));
            }
        };

        // parse rhs with right-associativity
        let value_idx = self.pratt_parse_expression(BindingPower::Assignment)?;

        let value_node_cloned = self.node(&value_idx).ok_or_else(|| {
            crate::parse_err!(
                "Node not found: assignment value node",
                Some(self.current().span.to_display(self.interner))
            )
            .attach(Help("This is likely a parser bug - please report it".into()))
        })?;
        let assignment_span = target_node_cloned.span.connect_new(&value_node_cloned.span);

        Ok(self.push(Node::new(
            NodeKind::Expr {
                kind: ExprKind::CompoundAssign {
                    target: target_idx,
                    value: value_idx,
                    op,
                },
                type_: None,
            },
            assignment_span,
        )))
    }
}
