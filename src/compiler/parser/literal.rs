use generational_arena::Index;

use crate::compiler::tokens::{Token, TokenKind};

use super::{
    Parser,
    error::ParserError,
    node::{ExprKind, Literal, Node, NodeKind},
};

impl Parser {
    pub fn parse_literal_nud(&mut self, literal_token: Token) -> Result<Index, ParserError> {
        let literal_kind = match literal_token.kind {
            TokenKind::IntLiteral(i) => Literal::Int(i),
            TokenKind::FloatLiteral(f) => Literal::Float(f),
            TokenKind::BoolLiteral(b) => Literal::Bool(b),
            _ => {
                return Err(ParserError::InternalError("Not a literal token in parse_literal_nud".into()));
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
