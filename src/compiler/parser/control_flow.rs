use generational_arena::Index;

use crate::compiler::tokens::{Punctuation, Token, TokenKind};

use super::{
    Parser,
    error::ParserError,
    node::{ExprKind, Node, NodeKind, Type},
    precedence::BindingPower,
};

impl Parser {
    /// parses `let name [: type] = value` as an expression (nud for 'let' keyword)
    pub fn parse_let_expr_nud(&mut self, let_keyword_token: Token) -> Result<Index, ParserError> {
        self.advance(); // consume 'let'

        let name_token = *self.current();
        let name_symbol = match name_token.kind {
            TokenKind::Identifier(s) => s,
            _ => return Err(ParserError::ExpectedIdentifier { token: name_token }),
        };

        self.advance(); // consume identifier

        let mut type_annotation: Option<Type> = None;
        let mut current_span = let_keyword_token.span.connect_new(&name_token.span);

        if self.current().kind == TokenKind::Punctuation(Punctuation::Colon) {
            self.advance(); // consume ':'
            let type_node_token = *self.current();
            current_span.connect_mut(&type_node_token.span);
            let parsed_type = match type_node_token.kind {
                TokenKind::TypeLiteral(tl) => Type::Primitive(tl),
                TokenKind::Identifier(custom_type_name) => Type::Custom(custom_type_name),
                _ => return Err(ParserError::ExpectedType { token: type_node_token }),
            };
            type_annotation = Some(parsed_type);
            self.advance(); // Consumes type
        }

        if self.current().kind != TokenKind::Punctuation(Punctuation::Eq) {
            return Err(ParserError::LetInitializerExpected { token: *self.current() });
        }

        let eq_token = *self.current();
        current_span.connect_mut(&eq_token.span);
        self.advance(); // consume '='

        // the value is an expression, parse it with full precedence.
        let value_expr_idx = self.pratt_parse_expression(BindingPower::None)?;
        let value_expr_node_cloned = self
            .node(&value_expr_idx)
            .ok_or_else(|| ParserError::InternalError(format!("Node not found for let binding value at {:?}", self.current().span)))?;
        current_span.connect_mut(&value_expr_node_cloned.span);

        Ok(self.push(Node::new(
            NodeKind::Expr {
                kind: ExprKind::LetBinding {
                    name: name_symbol,
                    type_annotation,
                    value: value_expr_idx,
                },
                type_: None,
            },
            current_span,
        )))
    }

    pub fn parse_return_expr_nud(&mut self, return_keyword_token: Token) -> Result<Index, ParserError> {
        self.advance();

        let mut value_idx_opt = None;
        let mut current_span = return_keyword_token.span;

        if self.current().kind != TokenKind::Punctuation(Punctuation::Semicolon) {
            let value_idx = self.pratt_parse_expression(BindingPower::None)?;
            let value_node = self
                .node(&value_idx)
                .ok_or_else(|| ParserError::InternalError("Return value node not found".to_string()))?
                .clone();
            current_span.connect_mut(&value_node.span);
            value_idx_opt = Some(value_idx);
        }

        Ok(self.push(Node::new(
            NodeKind::Expr {
                kind: ExprKind::Return { value: value_idx_opt },
                type_: None,
            },
            current_span,
        )))
    }

    pub fn parse_break_expr_nud(&mut self, break_keyword_token: Token) -> Result<Index, ParserError> {
        self.advance();

        Ok(self.push(Node::new(
            NodeKind::Expr {
                kind: ExprKind::Break,
                type_: None,
            },
            break_keyword_token.span,
        )))
    }

    pub fn parse_loop_expr_nud(&mut self, loop_keyword_token: Token) -> Result<Index, ParserError> {
        self.advance();

        if self.current().kind != TokenKind::Punctuation(Punctuation::OpenBrace) {
            return Err(ParserError::LoopBodyExpected { token: *self.current() });
        }
        let body_idx = self.parse_block_body()?;

        let body_node = self
            .node(&body_idx)
            .ok_or_else(|| ParserError::InternalError("Loop body node not found".to_string()))?;
        let loop_span = loop_keyword_token.span.connect_new(&body_node.span);

        Ok(self.push(Node::new(
            NodeKind::Expr {
                kind: ExprKind::Loop { body: body_idx },
                type_: None,
            },
            loop_span,
        )))
    }
}