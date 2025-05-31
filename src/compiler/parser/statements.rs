use generational_arena::Index;
use string_interner::symbol::SymbolUsize;

use crate::compiler::{
    parser::Parser,
    tokens::{Keyword, Punctuation, TokenKind},
};

use super::{
    error::ParserError,
    node::{Node, NodeKind, Stmt, StmtKind, Type},
};

impl Parser {
    /// Wrapper around the other statement parsing functions, does not have a Node of its own
    pub fn parse_statement(&mut self) -> Result<Index, ParserError> {
        let stmt = match self.current().kind {
            TokenKind::Keyword(Keyword::Let) => self.parse_let_stmt(),
            TokenKind::Identifier(name) => {
                self.advance();
                match self.current().kind {
                    TokenKind::Punctuation(Punctuation::Eq) => self.parse_assignment_stmt(name),
                    TokenKind::Punctuation(Punctuation::OpenBrace) => self.parse_call(name),
                    TokenKind::Punctuation(Punctuation::Dot) => self.parse_method_call(name),
                    _ => {
                        return Err(ParserError::UnexpectedToken {
                            token: self.current().clone(),
                            allowed: vec![
                                TokenKind::Punctuation(Punctuation::Eq),
                                TokenKind::Punctuation(Punctuation::OpenBrace),
                                TokenKind::Punctuation(Punctuation::Dot),
                            ],
                        })
                    }
                }
            }
            _ => {
                return Err(ParserError::UnexpectedToken {
                    token: self.current().clone(),
                    allowed: vec![TokenKind::Keyword(Keyword::Let)],
                })
            }
        };

        self.advance();

        match self.current().kind {
            TokenKind::Punctuation(Punctuation::Semicolon) => {
                self.advance();
                stmt
            }
            _ => {
                return Err(ParserError::UnexpectedToken {
                    token: self.current().clone(),
                    allowed: vec![TokenKind::Punctuation(Punctuation::Semicolon)],
                })
            }
        }
    }

    fn parse_let_stmt(&mut self) -> Result<Index, ParserError> {
        let start_span = self.current().span;
        self.advance();

        let name = match self.current().kind {
            TokenKind::Identifier(name) => name,
            _ => {
                return Err(ParserError::ExpectedIdentifier {
                    token: self.current().clone(),
                })
            }
        };

        self.advance();

        let mut type_ = None;
        let mut expr = None;
        match self.current().kind {
            // let x: i32 = 1;
            TokenKind::Punctuation(Punctuation::Colon) => {
                self.advance();
                let var_type_ = match self.current().kind {
                    TokenKind::TypeLiteral(type_) => Type::Primitive(type_),
                    TokenKind::Identifier(name) => Type::Custom(name),
                    _ => {
                        return Err(ParserError::ExpectedType {
                            token: self.current().clone(),
                        })
                    }
                };
                type_ = Some(var_type_);
                self.advance();
            }

            // let x = 1;
            TokenKind::Punctuation(Punctuation::Eq) => {
                self.advance();
                expr = match self.parse_expr() {
                    Ok(expr) => Some(expr),
                    Err(e) => {
                        self.parse_errors.push(e);
                        None
                    }
                };
            }

            // let x;
            TokenKind::Punctuation(Punctuation::Semicolon) => {
                // expr is already None
                self.advance();
            }
            _ => {
                return Err(ParserError::UnexpectedToken {
                    token: self.current().clone(),
                    allowed: vec![
                        TokenKind::Punctuation(Punctuation::Colon),
                        TokenKind::Punctuation(Punctuation::Eq),
                    ],
                })
            }
        }

        Ok(self.push(Node::new(
            NodeKind::Stmt(Stmt {
                span: start_span.connect_new(&self.current().span),
                kind: StmtKind::Let {
                    name,
                    type_,
                    init_value: expr,
                },
            }),
            self.current().span,
        )))
    }

    fn parse_assignment_stmt(&mut self, name: SymbolUsize) -> Result<Index, ParserError> {
        todo!()
    }
}
