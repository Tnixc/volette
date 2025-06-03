use generational_arena::Index;

use crate::compiler::tokens::{Punctuation, TokenKind};

use super::{
    error::ParserError,
    node::{ExprKind, Node, NodeKind},
    Parser,
};

impl Parser {
    pub fn parse_block_body(&mut self) -> Result<Index, ParserError> {
        let start_span = self.current().span;

        self.advance();

        let mut nodes = Vec::new();

        while self.current().kind != TokenKind::Punctuation(Punctuation::CloseBrace)
            && self.current().kind != TokenKind::Eof
        {
            let node = self.parse_expr()?;
            nodes.push(node);
        }

        if self.current().kind != TokenKind::Punctuation(Punctuation::CloseBrace) {
            return Err(ParserError::BlockExpectedCloseBrace { token: *self.current() });
        }

        let end_span = self.current().span;

        self.advance();

        Ok(self.push(Node::new(
            NodeKind::Expr {
                kind: ExprKind::Block { exprs: nodes },
                type_: None,
            },
            start_span.connect_new(&end_span),
        )))
    }
}
