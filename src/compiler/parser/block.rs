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

        while self.current().kind != TokenKind::Punctuation(Punctuation::CloseBrace) {
            //     let node = self.parse_stmt()?;
            //     nodes.push(node);
            println!("{:?}", self.current());
            println!();
            self.advance();
        }

        println!("done");
        self.advance();

        Ok(self.push(Node::new(
            NodeKind::Expr(ExprKind::Block { exprs: nodes }),
            self.current().span,
        )))
    }
}
