use generational_arena::Index;

use crate::compiler::tokens::{Punctuation, Token, TokenKind};

use super::{
    Parser,
    error::ParserError,
    precedence::BindingPower,
};

impl Parser {
    pub fn parse_paren_expr_nud(&mut self, open_paren_token: Token) -> Result<Index, ParserError> {
        self.advance(); // consume '('
        let inner_expr_idx = self.pratt_parse_expression(BindingPower::None)?; // Parse expression inside parentheses

        if self.current().kind != TokenKind::Punctuation(Punctuation::CloseParen) {
            return Err(ParserError::CloseParenExpected { token: *self.current() });
        }
        let close_paren_token = *self.current();
        self.advance(); // consume ')'

        let full_span = open_paren_token.span.connect_new(&close_paren_token.span);

        let actual_inner_node = self
            .tree
            .get_mut(inner_expr_idx)
            .ok_or_else(|| ParserError::InternalError("Inner expression node not found in parentheses".to_string()))?;
        actual_inner_node.span = full_span; // update span to include parentheses
        Ok(inner_expr_idx)
    }

    pub fn parse_block_expr_nud(&mut self, _open_brace_token: Token) -> Result<Index, ParserError> {
        self.parse_block_body()
    }
}