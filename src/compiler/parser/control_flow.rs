use generational_arena::Index;

use crate::compiler::tokens::{Keyword::Else, Punctuation, Token, TokenKind};

use super::{
    Parser,
    error::ParserError,
    node::{ExprKind, Node, NodeKind, Type},
    precedence::BindingPower,
};

impl<'a> Parser<'a> {
    /// parses `let name [: type] = value` as an expression (nud for 'let' keyword)
    pub fn parse_let_expr_nud(&mut self, let_keyword_token: Token) -> Result<Index, ParserError> {
        self.advance(); // consume 'let'

        let name_token = *self.current();
        let name_symbol = match name_token.kind {
            TokenKind::Identifier(s) => s,
            _ => {
                return Err(ParserError::Expected {
                    what: "identifier after 'let'".to_string(),
                    got: format!("{:?}", name_token.kind),
                    span: name_token.span.to_display(self.interner),
                });
            }
        };

        self.advance(); // consume identifier

        let mut type_annotation: Option<Type> = None;
        let mut current_span = let_keyword_token.span.connect_new(&name_token.span);

        if self.current().kind == TokenKind::Punctuation(Punctuation::Colon) {
            self.advance(); // consume ':'
            let (parsed_type, type_span) = self.parse_type()?;
            current_span.connect_mut(&type_span);
            type_annotation = Some(parsed_type);
        }

        if self.current().kind != TokenKind::Punctuation(Punctuation::Eq) {
            return Err(ParserError::Expected {
                what: "initializer '='".to_string(),
                got: format!("{:?}", self.current().kind),
                span: self.current().span.to_display(self.interner),
            });
        }

        let eq_token = *self.current();
        current_span.connect_mut(&eq_token.span);
        self.advance(); // consume '='

        // the value is an expression, parse it with full precedence.
        let value_expr_idx = self.pratt_parse_expression(BindingPower::None)?;
        let value_expr_node_cloned = self.node(&value_expr_idx).ok_or_else(|| ParserError::NotFound {
            what: "let binding value node".to_string(),
            span: self.current().span.to_display(self.interner),
        })?;
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
                .ok_or_else(|| ParserError::NotFound {
                    what: "return value node".to_string(),
                    span: self.current().span.to_display(self.interner),
                })?
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
            return Err(ParserError::Expected {
                what: "loop body (opening brace)".to_string(),
                got: format!("{:?}", self.current().kind),
                span: self.current().span.to_display(self.interner),
            });
        }
        let body_idx = self.parse_block_body()?;

        let body_node = self.node(&body_idx).ok_or_else(|| ParserError::NotFound {
            what: "loop body node".to_string(),
            span: self.current().span.to_display(self.interner),
        })?;
        let loop_span = loop_keyword_token.span.connect_new(&body_node.span);

        Ok(self.push(Node::new(
            NodeKind::Expr {
                kind: ExprKind::Loop { body: body_idx },
                type_: None,
            },
            loop_span,
        )))
    }

    pub fn parse_if_expr_nud(&mut self, if_keyword_token: Token) -> Result<Index, ParserError> {
        self.advance(); // consume 'if'

        let condition_idx = self.pratt_parse_expression(BindingPower::None)?;

        let then_block_idx = self.parse_block_body()?;
        let then_block_node = self.node(&then_block_idx).ok_or_else(|| ParserError::NotFound {
            what: "if body node".to_string(),
            span: self.current().span.to_display(self.interner),
        })?;
        let mut span = if_keyword_token.span.connect_new(&then_block_node.span);

        let mut else_block_idx_opt = None;
        if self.current().kind == TokenKind::Keyword(Else) {
            self.advance(); // consume 'else'
            let else_block_idx = self.parse_block_body()?;
            let else_block_node = self.node(&else_block_idx).ok_or_else(|| ParserError::NotFound {
                what: "else body node".to_string(),
                span: self.current().span.to_display(self.interner),
            })?;
            span.connect_mut(&else_block_node.span);
            else_block_idx_opt = Some(else_block_idx);
        }

        Ok(self.push(Node::new(
            NodeKind::Expr {
                kind: ExprKind::If {
                    cond: condition_idx,
                    then_block: then_block_idx,
                    else_block: else_block_idx_opt,
                },
                type_: None,
            },
            span,
        )))
    }
}
