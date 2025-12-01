use generational_arena::Index;

use crate::compiler::{
    error::Help,
    tokens::{Punctuation, TokenKind},
};
use rootcause::prelude::*;

use super::{
    Parser,
    node::{ExprKind, Node, NodeKind},
};

impl<'a> Parser<'a> {
    pub fn parse_block_body(&mut self) -> Result<Index, Report> {
        let start_span = self.current().span;

        self.advance(); // consume '{'

        let mut expressions = Vec::new();

        while self.current().kind != TokenKind::Punctuation(Punctuation::CloseBrace) && self.current().kind != TokenKind::Eof {
            let expr = self.parse_expr()?;
            expressions.push(expr);

            while self.current().kind == TokenKind::Punctuation(Punctuation::Semicolon) {
                self.advance();
            }
        }

        if self.current().kind != TokenKind::Punctuation(Punctuation::CloseBrace) {
            return Err(crate::parse_err!(
                "Expected closing brace '}}', got {:?}",
                Some(self.current().span.to_display(self.interner)),
                self.current().kind
            )
            .attach(Help("Check the syntax - the parser expected something different here".into())));
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
