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
        // Specific statements that are not expressions (or have distinct keywords)
        match self.current().kind {
            TokenKind::Keyword(Keyword::Return) => self.parse_return_stmt(),
            TokenKind::Keyword(Keyword::Break) => self.parse_break_stmt(), // Assuming break is a statement
            TokenKind::Keyword(Keyword::Loop) => self.parse_loop_stmt(),   // Assuming loop is a statement
            // Add other distinct statements like `free`, etc. if they are not expressions
            // TokenKind::Keyword(Keyword::Free) => self.parse_free_stmt(),
            _ => {
                // Default to parsing an expression statement
                let expr_idx = self.parse_expr()?; // This now handles let, assignment, etc.
                let expr_node = self
                    .node(&expr_idx)
                    .ok_or_else(|| ParserError::InternalError("Expression node not found for statement".to_string()))?
                    .clone();

                if self.current().kind == TokenKind::Punctuation(Punctuation::Semicolon) {
                    let semicolon_token = self.current().clone();
                    self.advance(); // Consume semicolon

                    let stmt_span = expr_node.span.connect_new(&semicolon_token.span);
                    Ok(self.push(Node::new(
                        NodeKind::Stmt {
                            kind: StmtKind::ExpressionStmt { expr: expr_idx },
                        },
                        stmt_span,
                    )))
                } else {
                    // In some contexts, the last expression in a block might not need a semicolon.
                    // For a general statement list, it's usually an error.
                    Err(ParserError::SemicolonExpectedAfterExpressionStatement {
                        token: self.current().clone(),
                    })
                }
            }
        }
    }

    // Implement or adjust these statement parsers
    fn parse_return_stmt(&mut self) -> Result<Index, ParserError> {
        let return_keyword_token = self.current().clone();
        self.advance(); // Consume 'return'

        let mut value_idx_opt = None;
        let mut current_span = return_keyword_token.span;

        // Check if there's a value to return or if it's just 'return;'
        if self.current().kind != TokenKind::Punctuation(Punctuation::Semicolon) {
            // TODO: Check if current token can start an expression.
            // This check is basic. A better check is to try parsing an expression
            // and if it fails, assume no expression was intended IF a semicolon follows.
            // For now, if not semicolon, assume expression.
            let value_idx = self.parse_expr()?;
            let value_node = self.node(&value_idx).unwrap().clone();
            current_span.connect_mut(&value_node.span);
            value_idx_opt = Some(value_idx);
        }

        // Expect semicolon
        if self.current().kind != TokenKind::Punctuation(Punctuation::Semicolon) {
            return Err(ParserError::SemicolonExpectedAfterExpressionStatement {
                token: self.current().clone(),
            });
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
        let break_keyword_token = self.current().clone();
        self.advance(); // Consume 'break'
        let mut current_span = break_keyword_token.span;

        // Expect semicolon
        if self.current().kind != TokenKind::Punctuation(Punctuation::Semicolon) {
            return Err(ParserError::SemicolonExpectedAfterExpressionStatement {
                token: self.current().clone(),
            });
        }
        current_span.connect_mut(&self.current().span);
        self.advance(); // consume semicolon

        Ok(self.push(Node::new(NodeKind::Stmt { kind: StmtKind::Break }, current_span)))
    }

    fn parse_loop_stmt(&mut self) -> Result<Index, ParserError> {
        let loop_keyword_token = self.current().clone();
        self.advance(); // Consume 'loop'

        // Expect a block for the loop body
        if self.current().kind != TokenKind::Punctuation(Punctuation::OpenBrace) {
            // Or however your language defines loop bodies if not blocks
            return Err(ParserError::FnBodyExpected {
                token: self.current().clone(),
            }); // Reusing error, better specific one
        }
        let body_idx = self.parse_block()?; // Assuming parse_block returns Index of the block Node

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
