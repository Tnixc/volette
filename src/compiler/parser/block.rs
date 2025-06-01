use generational_arena::Index;

use crate::compiler::tokens::{Punctuation, TokenKind};

use super::{
    error::ParserError,
    node::{ExprKind, Node, NodeKind},
    Parser,
};

impl Parser {
    pub fn parse_block(&mut self) -> Result<Index, ParserError> {
        let start_span = self.current().span;
        let mut nodes = Vec::new();

        while self
            .advance()
            .is_some_and(|t| t.kind != TokenKind::Punctuation(Punctuation::CloseBrace))
        {
            let node = self.parse_expr()?;
            nodes.push(node);
        }

        match self.current().kind {
            TokenKind::Punctuation(Punctuation::CloseBrace) => {}
            _ => return Err(ParserError::BlockExpectedCloseBrace { token: *self.current() }),
        }

        Ok(self.push(Node::new(
            NodeKind::Expr {
                kind: ExprKind::Block { exprs: nodes },
                type_: None,
            },
            start_span.connect_new(&self.current().span),
        )))
    }
}
