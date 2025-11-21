use generational_arena::Index;
use string_interner::symbol::SymbolUsize;

use crate::compiler::{
    error::Help,
    tokens::{Punctuation, Token, TokenKind},
};
use rootcause::prelude::*;

use super::{
    Parser,
    node::{ExprKind, Node, NodeKind},
    precedence::BindingPower,
};

impl<'a> Parser<'a> {
    pub fn parse_identifier_nud(&mut self, ident_token: Token) -> Result<Index, Report> {
        let name_symbol = match ident_token.kind {
            TokenKind::Identifier(s) => s,
            _ => {
                return Err(crate::parse_err!(
                    "Invalid token in identifier context: Expected identifier, got {:?}",
                    Some(ident_token.span.to_display(self.interner)),
                    ident_token.kind
                )
                .attach(Help("This construct is not valid in the current context".into())));
            }
        };

        self.advance(); // consume the identifier token

        // check if this is a function call
        if self.current().kind == TokenKind::Punctuation(Punctuation::OpenParen) {
            self.parse_function_call(ident_token, name_symbol)
        } else {
            Ok(self.push(Node::new(
                NodeKind::Expr {
                    kind: ExprKind::Identifier(name_symbol),
                    type_: None,
                },
                ident_token.span,
            )))
        }
    }

    pub fn parse_function_call(&mut self, ident_token: Token, name_symbol: SymbolUsize) -> Result<Index, Report> {
        let mut call_span = ident_token.span;

        self.advance(); // consume '('

        let mut args = Vec::new();

        // handle empty argument list
        if self.current().kind == TokenKind::Punctuation(Punctuation::CloseParen) {
            let close_paren_token = *self.current();
            call_span.connect_mut(&close_paren_token.span);
            self.advance(); // consume ')'
        } else {
            // parse arguments
            loop {
                let arg_expr = self.pratt_parse_expression(BindingPower::None)?;
                args.push(arg_expr);

                match self.current().kind {
                    TokenKind::Punctuation(Punctuation::Comma) => {
                        self.advance(); // consume ','
                        continue;
                    }
                    TokenKind::Punctuation(Punctuation::CloseParen) => {
                        let close_paren_token = *self.current();
                        call_span.connect_mut(&close_paren_token.span);
                        self.advance(); // consume ')'
                        break;
                    }
                    _ => {
                        return Err(crate::parse_err!(
                            "Expected closing parenthesis or comma, got {:?}",
                            Some(self.current().span.to_display(self.interner)),
                            self.current().kind
                        )
                        .attach(Help("Check the syntax - the parser expected something different here".into())));
                    }
                }
            }
        }

        let func_node = Node::new(
            NodeKind::Expr {
                kind: ExprKind::Identifier(name_symbol),
                type_: None,
            },
            ident_token.span,
        );
        let func_idx = self.push(func_node);

        Ok(self.push(Node::new(
            NodeKind::Expr {
                kind: ExprKind::Call { func: func_idx, args },
                type_: None,
            },
            call_span,
        )))
    }

    pub fn parse_call_led(&mut self, _open_paren_token: Token, func_idx: Index) -> Result<Index, Report> {
        let func_node = self
            .node(&func_idx)
            .ok_or_else(|| {
                crate::parse_err!(
                    "Node not found: function node for call",
                    Some(self.current().span.to_display(self.interner))
                )
                .attach(Help("This is likely a parser bug - please report it".into()))
            })?
            .clone();
        let mut call_span = func_node.span;

        let mut args = Vec::new();

        // handle empty argument list
        if self.current().kind == TokenKind::Punctuation(Punctuation::CloseParen) {
            let close_paren_token = *self.current();
            call_span.connect_mut(&close_paren_token.span);
            self.advance(); // consume ')'
        } else {
            loop {
                let arg_expr = self.pratt_parse_expression(BindingPower::None)?;
                args.push(arg_expr);

                match self.current().kind {
                    TokenKind::Punctuation(Punctuation::Comma) => {
                        self.advance(); // consume ','
                        continue;
                    }
                    TokenKind::Punctuation(Punctuation::CloseParen) => {
                        let close_paren_token = *self.current();
                        call_span.connect_mut(&close_paren_token.span);
                        self.advance(); // consume ')'
                        break;
                    }
                    _ => {
                        return Err(crate::parse_err!(
                            "Expected closing parenthesis or comma, got {:?}",
                            Some(self.current().span.to_display(self.interner)),
                            self.current().kind
                        )
                        .attach(Help("Check the syntax - the parser expected something different here".into())));
                    }
                }
            }
        }

        Ok(self.push(Node::new(
            NodeKind::Expr {
                kind: ExprKind::Call { func: func_idx, args },
                type_: None,
            },
            call_span,
        )))
    }
}
