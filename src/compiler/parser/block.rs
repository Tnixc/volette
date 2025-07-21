use generational_arena::Index;

use crate::compiler::tokens::{Punctuation, TokenKind};

use super::{
    Parser,
    error::ParserError,
    node::{ExprKind, Node, NodeKind},
};

impl Parser {
    pub fn parse_block_body(&mut self) -> Result<Index, ParserError> {
        let start_span = self.current().span;

        self.advance(); // consume '{'

        let mut expressions = Vec::new();

        while self.current().kind != TokenKind::Punctuation(Punctuation::CloseBrace)
            && self.current().kind != TokenKind::Eof
        {
            let expr = self.parse_expr()?;
            expressions.push(expr);

            // Handle optional semicolon after expression
            if self.current().kind == TokenKind::Punctuation(Punctuation::Semicolon) {
                self.advance(); // consume ';'
            }
        }

        if self.current().kind != TokenKind::Punctuation(Punctuation::CloseBrace) {
            return Err(ParserError::BlockExpectedCloseBrace { token: *self.current() });
        }

        let end_span = self.current().span;
        self.advance(); // consume '}'

        Ok(self.push(Node::new(
            NodeKind::Expr {
                kind: ExprKind::Block { exprs: expressions },
                type_: None,
            },
            start_span.connect_new(&end_span),
        )))
    }
}
