use generational_arena::Index;

use crate::compiler::tokens::{Punctuation, TokenKind};

use super::{
    error::ParserError,
    node::{ExprKind, Node, NodeKind},
    Parser,
};

impl Parser {
    pub fn parse_block(&mut self) -> Result<Index, ParserError> {
        let mut nodes = Vec::new();

        // TODO: depth
        while self
            .advance()
            .is_some_and(|t| t.kind != TokenKind::Punctuation(Punctuation::CloseBrace))
        {
            //     let node = self.parse_stmt()?;
            //     nodes.push(node);
        }

        match self.current().kind {
            TokenKind::Punctuation(Punctuation::CloseBrace) => {}
            _ => {
                return Err(ParserError::BlockExpectedCloseBrace {
                    token: self.current().clone(),
                })
            }
        }

        Ok(self.push(Node::new(
            NodeKind::Expr(ExprKind::Block { exprs: nodes }),
            self.current().span,
        )))
    }
}
