use generational_arena::Index;

use crate::compiler::tokens::{Punctuation, Token, TokenKind};

use super::{
    Parser,
    error::ParserError,
    node::{BinOpKind, ExprKind, Node, NodeKind},
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

    pub fn parse_compound_assignment_led(&mut self, op_token: Token, target_idx: Index) -> Result<Index, ParserError> {
        // Lvalue check (target must be assignable)
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
            _ => {
                return Err(ParserError::Invalid {
                    what: "assignment target".to_string(),
                    reason: "Only variables can be assigned to".to_string(),
                    span: target_node_cloned.span.to_display(&self.interner),
                });
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
                return Err(ParserError::Invalid {
                    what: "compound assignment operator".to_string(),
                    reason: format!("Unknown operator {:?}", op_token.kind),
                    span: op_token.span.to_display(&self.interner),
                });
            }
        };

        // parse rhs with right-associativity
        let value_idx = self.pratt_parse_expression(BindingPower::Assignment)?;

        let value_node_cloned = self.node(&value_idx).ok_or_else(|| ParserError::NotFound {
            what: "assignment value node".to_string(),
            span: self.current().span.to_display(self.interner),
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
