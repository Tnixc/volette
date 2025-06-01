use super::{
    error::ParserError,
    node::{ExprKind, Literal, Node, NodeKind, Type},
    Parser,
};
use crate::compiler::tokens::{Keyword, Punctuation, TokenKind};
use generational_arena::Index;

impl Parser {
    pub fn parse_expr(&mut self) -> Result<Index, ParserError> {
        self.parse_primary_expr()
    }

    fn parse_primary_expr(&mut self) -> Result<Index, ParserError> {
        let current_token = *self.current();
        match self.current().kind {
            TokenKind::Keyword(Keyword::Let) => self.parse_let_expr(),
            TokenKind::IntLiteral(i) => {
                self.advance();
                Ok(self.push(Node::new(
                    NodeKind::Expr {
                        kind: ExprKind::Literal(Literal::Int(i)),
                        type_: None,
                    },
                    current_token.span,
                )))
            }
            TokenKind::FloatLiteral(f) => {
                self.advance();
                Ok(self.push(Node::new(
                    NodeKind::Expr {
                        kind: ExprKind::Literal(Literal::Float(f)),
                        type_: None,
                    },
                    current_token.span,
                )))
            }
            TokenKind::Identifier(name) => {
                self.advance();
                Ok(self.push(Node::new(
                    NodeKind::Expr {
                        kind: ExprKind::Identifier(name),
                        type_: None,
                    },
                    current_token.span,
                )))
            }
            TokenKind::Punctuation(Punctuation::OpenParen) => {
                self.advance(); // consume '('
                let expr_idx = self.parse_expr()?;
                if self.current().kind != TokenKind::Punctuation(Punctuation::CloseParen) {
                    return Err(ParserError::CloseParenExpected { token: *self.current() });
                }
                let close_paren_span = self.current().span;
                self.advance(); // Consume ')'

                let full_span = current_token.span.connect_new(&close_paren_span);
                let paren_expr_node = Node::new(
                    NodeKind::Expr {
                        kind: ExprKind::ParenExpr { expr: expr_idx },
                        type_: None,
                    },
                    full_span,
                );
                Ok(self.push(paren_expr_node))
            }
            // TODO: Add other primary expressions: block, if, call
            _ => Err(ParserError::ExpressionExpected { token: current_token }),
        }
    }

    // assignment: target = value
    // right-associative.
    fn parse_assignment_expr(&mut self) -> Result<Index, ParserError> {
        let lhs_expr_idx = self.parse_binary_op(0)?;

        if self.current().kind == TokenKind::Punctuation(Punctuation::Eq) {
            let lhs_node = self
                .node(&lhs_expr_idx)
                .ok_or_else(|| ParserError::InternalError("LHS node not found for assignment check".to_string()))?
                .clone();

            // check if LHS is a valid assignment target (L-value)
            match lhs_node.kind {
                NodeKind::Expr {
                    kind: ExprKind::Identifier(_),
                    ..
                } => {}
                // TODO: other valid L-values like field access
                _ => {
                    return Err(ParserError::InvalidLHSInAssignment {
                        span: lhs_node.span.to_display(&self.interner),
                    });
                }
            }

            self.advance(); // Consume '='
            let rhs_expr_idx = self.parse_assignment_expr()?; // recursive call for right-associativity (e.g., a = b = c)

            let rhs_node = self
                .node(&rhs_expr_idx)
                .ok_or_else(|| ParserError::InternalError("RHS node not found for assignment".to_string()))?
                .clone();

            let assignment_span = lhs_node.span.connect_new(&rhs_node.span);
            let assignment_type = rhs_node.type_;

            return Ok(self.push(Node::new(
                NodeKind::Expr {
                    kind: ExprKind::Assign {
                        target: lhs_expr_idx,
                        value: rhs_expr_idx,
                    },
                    type_: assignment_type,
                },
                assignment_span,
            )));
        }
        Ok(lhs_expr_idx)
    }

    // pratt soon tm
    fn parse_binary_op(&mut self, _min_precedence: u8) -> Result<Index, ParserError> {
        todo!();
    }

    // parses `let name : type = value`, where : type is optional
    pub fn parse_let_expr(&mut self) -> Result<Index, ParserError> {
        let let_keyword_token = *self.current();
        self.advance(); // consumes 'let'

        let name_token = *self.current();
        let name_symbol = match name_token.kind {
            TokenKind::Identifier(s) => s,
            _ => return Err(ParserError::ExpectedIdentifier { token: name_token }),
        };
        self.advance(); // consumes identifier

        let mut type_annotation: Option<Type> = None;
        let mut current_span = let_keyword_token.span.connect_new(&name_token.span);

        // optional type annotation
        if self.current().kind == TokenKind::Punctuation(Punctuation::Colon) {
            self.advance(); // consumes ':'
            let type_node_token = *self.current();
            current_span.connect_mut(&type_node_token.span);
            let parsed_type = match type_node_token.kind {
                TokenKind::TypeLiteral(tl) => Type::Primitive(tl),
                TokenKind::Identifier(custom_type_name) => Type::Custom(custom_type_name),
                _ => return Err(ParserError::ExpectedType { token: type_node_token }),
            };
            type_annotation = Some(parsed_type);
            self.advance(); // consumes type
        }

        if self.current().kind != TokenKind::Punctuation(Punctuation::Eq) {
            return Err(ParserError::LetInitializerExpected { token: *self.current() });
        }

        let eq_token = *self.current();
        current_span.connect_mut(&eq_token.span);
        self.advance(); // consumes '='

        let value_expr_idx = self.parse_expr()?;
        let value_expr_node = self.node(&value_expr_idx).ok_or_else(|| ParserError::ExpectedNode {
            span: self.current().span.to_display(&self.interner),
        })?;

        current_span.connect_mut(&value_expr_node.span);

        Ok(self.push(Node::new(
            NodeKind::Expr {
                kind: ExprKind::LetBinding {
                    name: name_symbol,
                    type_annotation,
                    value: value_expr_idx,
                },
                type_: value_expr_node.type_, // let expr type is the value's type
            },
            current_span,
        )))
    }
}
