use generational_arena::Index;
// Ensure necessary imports are here or handled by `mod.rs`
use super::{
    error::ParserError,
    node::{Node, NodeKind, StmtKind}, // Type might not be directly used here anymore
    Parser,
};
use crate::compiler::tokens::{Keyword, Punctuation, TokenKind};

impl Parser {
    pub fn parse_statement(&mut self) -> Result<Index, ParserError> {
        match self.current().kind {
            TokenKind::Keyword(Keyword::Return) => self.parse_return_stmt(),
            TokenKind::Keyword(Keyword::Break) => self.parse_break_stmt(),
            TokenKind::Keyword(Keyword::Loop) => self.parse_loop_stmt(),
            _ => {
                let expr_idx = self.parse_expr()?;
                let expr_node = self
                    .node(&expr_idx)
                    .ok_or_else(|| ParserError::InternalError("Expression node not found for statement".to_string()))?
                    .clone();

                if self.current().kind == TokenKind::Punctuation(Punctuation::Semicolon) {
                    let semicolon_token = *self.current();
                    self.advance(); // consume semicolon

                    let stmt_span = expr_node.span.connect_new(&semicolon_token.span);
                    Ok(self.push(Node::new(
                        NodeKind::Stmt {
                            kind: StmtKind::ExpressionStmt { expr: expr_idx },
                        },
                        stmt_span,
                    )))
                } else {
                    Err(ParserError::SemicolonExpectedAfterExpressionStatement { token: *self.current() })
                }
            }
        }
    }

    fn parse_return_stmt(&mut self) -> Result<Index, ParserError> {
        let return_keyword_token = *self.current();
        self.advance(); // Consume 'return'

        let mut value_idx_opt = None;
        let mut current_span = return_keyword_token.span;

        if self.current().kind != TokenKind::Punctuation(Punctuation::Semicolon) {
            let value_idx = self.parse_expr()?;
            let value_node = self.node(&value_idx).unwrap().clone();
            current_span.connect_mut(&value_node.span);
            value_idx_opt = Some(value_idx);
        }

        if self.current().kind != TokenKind::Punctuation(Punctuation::Semicolon) {
            return Err(ParserError::SemicolonExpectedAfterExpressionStatement { token: *self.current() });
        }

        current_span.connect_mut(&self.current().span);
        self.advance(); // consume semicolon

        Ok(self.push(Node::new(
            NodeKind::Stmt {
                kind: StmtKind::Return { value: value_idx_opt },
            },
            current_span,
        )))
    }

    fn parse_break_stmt(&mut self) -> Result<Index, ParserError> {
        let break_keyword_token = *self.current();
        self.advance(); // consume 'break'
        let mut current_span = break_keyword_token.span;

        if self.current().kind != TokenKind::Punctuation(Punctuation::Semicolon) {
            return Err(ParserError::SemicolonExpectedAfterExpressionStatement { token: *self.current() });
        }
        current_span.connect_mut(&self.current().span);
        self.advance(); // consume semicolon

        Ok(self.push(Node::new(NodeKind::Stmt { kind: StmtKind::Break }, current_span)))
    }

    fn parse_loop_stmt(&mut self) -> Result<Index, ParserError> {
        let loop_keyword_token = *self.current();
        self.advance(); // Consume 'loop'

        if self.current().kind != TokenKind::Punctuation(Punctuation::OpenBrace) {
            return Err(ParserError::LoopBodyExpected { token: *self.current() });
        }
        let body_idx = self.parse_block_body()?;

        let body_node = self.node(&body_idx).unwrap();
        let loop_span = loop_keyword_token.span.connect_new(&body_node.span);

        Ok(self.push(Node::new(
            NodeKind::Stmt {
                kind: StmtKind::Loop { body: body_idx },
            },
            loop_span,
        )))
    }
}
