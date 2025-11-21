use generational_arena::Index;

use crate::compiler::{
    error::Help,
    tokens::{Punctuation, Token, TokenKind},
};
use rootcause::prelude::*;

use super::{Parser, precedence::BindingPower};

impl<'a> Parser<'a> {
    pub fn parse_paren_expr_nud(&mut self, open_paren_token: Token) -> Result<Index, Report> {
        self.advance(); // consume '('
        let inner_expr_idx = self.pratt_parse_expression(BindingPower::None)?; // Parse expression inside parentheses

        if self.current().kind != TokenKind::Punctuation(Punctuation::CloseParen) {
            return Err(crate::parse_err!(
                "Expected closing parenthesis, got {:?}",
                Some(self.current().span.to_display(self.interner)),
                self.current().kind
            )
            .attach(Help("Check the syntax - the parser expected something different here".into())));
        }
        let close_paren_token = *self.current();
        self.advance(); // consume ')'

        let full_span = open_paren_token.span.connect_new(&close_paren_token.span);

        // get span before mutable borrow
        let error_span = self.current().span.to_display(self.interner);

        let actual_inner_node = self.tree.get_mut(inner_expr_idx).ok_or_else(|| {
            crate::parse_err!("Node not found: inner expression node in parentheses", Some(error_span))
                .attach(Help("This is likely a parser bug - please report it".into()))
        })?;
        actual_inner_node.span = full_span; // update span to include parentheses
        Ok(inner_expr_idx)
    }

    pub fn parse_block_expr_nud(&mut self, _open_brace_token: Token) -> Result<Index, Report> {
        self.parse_block_body()
    }
}
