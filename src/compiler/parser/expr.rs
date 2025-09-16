use std::ops::Add;

use super::{
    Parser,
    error::ParserError,
    node::{BinOpKind, ExprKind, Literal, Node, NodeKind, Type},
};
use crate::compiler::tokens::{Keyword, Punctuation, Token, TokenKind};
use generational_arena::Index;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(u32)]
enum BindingPower {
    None = 0,
    Assignment = 1,    // = (right-associative)
    LogicalOr = 2,     // ||
    LogicalAnd = 3,    // &&
    Equality = 4,      // == !=
    Comparison = 5,    // < > <= >=
    Term = 6,          // + -
    Factor = 7,        // * / %
    Unary = 8,         // Prefix - !
    Call = 9,          // () (function call)
    MemberAccess = 10, // . (field access)
    Primary = 11,      // Literals, identifiers
    More(u32),
}

impl Parser {
    pub fn parse_expr(&mut self) -> Result<Index, ParserError> {
        self.pratt_parse_expression(BindingPower::None)
    }

    /// the core pratt parsing loop.
    /// parses an expression whose components have at least `min_bp` binding power.
    fn pratt_parse_expression(&mut self, min_bp: BindingPower) -> Result<Index, ParserError> {
        // 1. handle nud (null denotation) for the current token (prefix context)
        let mut left_expr_idx: Index;
        let current_token = *self.current();

        match current_token.kind {
            TokenKind::IntLiteral(_) | TokenKind::FloatLiteral(_) | TokenKind::BoolLiteral(_) => {
                left_expr_idx = self.parse_literal_nud(current_token)?;
            }
            TokenKind::Identifier(_) => {
                left_expr_idx = self.parse_identifier_nud(current_token)?;
            }
            TokenKind::Keyword(Keyword::Let) => {
                left_expr_idx = self.parse_let_expr_nud(current_token)?;
            }
            TokenKind::Keyword(Keyword::Return) => {
                left_expr_idx = self.parse_return_expr_nud(current_token)?;
            }
            TokenKind::Keyword(Keyword::Break) => {
                left_expr_idx = self.parse_break_expr_nud(current_token)?;
            }
            TokenKind::Keyword(Keyword::Loop) => {
                left_expr_idx = self.parse_loop_expr_nud(current_token)?;
            }
            TokenKind::Punctuation(Punctuation::OpenParen) => {
                left_expr_idx = self.parse_paren_expr_nud(current_token)?;
            }
            TokenKind::Punctuation(Punctuation::OpenBrace) => {
                left_expr_idx = self.parse_block_expr_nud(current_token)?;
            }
            _ => {
                return Err(ParserError::ExpressionExpected { token: current_token });
            }
        }

        // 2. handle led (left denotation) for subsequent tokens (infix/postfix context)
        loop {
            let next_token = *self.current();
            let (left_bp, is_right_associative) = match next_token.kind {
                TokenKind::Punctuation(Punctuation::Eq) => (BindingPower::Assignment, true),

                TokenKind::Punctuation(Punctuation::Plus) | TokenKind::Punctuation(Punctuation::Minus) => (BindingPower::Term, false),

                TokenKind::Punctuation(Punctuation::Star)
                | TokenKind::Punctuation(Punctuation::Slash)
                | TokenKind::Punctuation(Punctuation::Percent) => (BindingPower::Factor, false),

                TokenKind::Punctuation(Punctuation::EqEq) | TokenKind::Punctuation(Punctuation::NotEq) => (BindingPower::Equality, false),

                TokenKind::Punctuation(Punctuation::LessThan)
                | TokenKind::Punctuation(Punctuation::LessThanOrEq)
                | TokenKind::Punctuation(Punctuation::GreaterThan)
                | TokenKind::Punctuation(Punctuation::GreaterThanOrEq) => (BindingPower::Comparison, false),

                TokenKind::Punctuation(Punctuation::AmpAmp) => (BindingPower::LogicalAnd, false),
                TokenKind::Punctuation(Punctuation::PipePipe) => (BindingPower::LogicalOr, false),

                TokenKind::Punctuation(Punctuation::OpenParen) => (BindingPower::Call, false),

                _ => (BindingPower::None, false),
            };

            if left_bp < min_bp || left_bp == BindingPower::None {
                break;
            }

            self.advance();

            match next_token.kind {
                TokenKind::Punctuation(Punctuation::Eq) => {
                    left_expr_idx = self.parse_assignment_led(next_token, left_expr_idx, is_right_associative)?;
                }
                TokenKind::Punctuation(Punctuation::Plus)
                | TokenKind::Punctuation(Punctuation::Minus)
                | TokenKind::Punctuation(Punctuation::Star)
                | TokenKind::Punctuation(Punctuation::Slash)
                | TokenKind::Punctuation(Punctuation::Percent)
                | TokenKind::Punctuation(Punctuation::EqEq)
                | TokenKind::Punctuation(Punctuation::NotEq)
                | TokenKind::Punctuation(Punctuation::LessThan)
                | TokenKind::Punctuation(Punctuation::LessThanOrEq)
                | TokenKind::Punctuation(Punctuation::GreaterThan)
                | TokenKind::Punctuation(Punctuation::GreaterThanOrEq)
                | TokenKind::Punctuation(Punctuation::AmpAmp)
                | TokenKind::Punctuation(Punctuation::PipePipe) => {
                    left_expr_idx = self.parse_binary_infix_op_led(next_token, left_expr_idx, left_bp, is_right_associative)?;
                }
                TokenKind::Punctuation(Punctuation::OpenParen) => {
                    left_expr_idx = self.parse_call_led(next_token, left_expr_idx)?;
                }

                _ => {
                    return Err(ParserError::InternalError(format!("Unhandled LED token: {:?}", next_token.kind)));
                }
            }
        }
        Ok(left_expr_idx)
    }

    fn parse_literal_nud(&mut self, literal_token: Token) -> Result<Index, ParserError> {
        let literal_kind = match literal_token.kind {
            TokenKind::IntLiteral(i) => Literal::Int(i),
            TokenKind::FloatLiteral(f) => Literal::Float(f),
            TokenKind::BoolLiteral(b) => Literal::Bool(b),
            _ => {
                return Err(ParserError::InternalError("Not a literal token in parse_literal_nud".into()));
            }
        };

        self.advance(); // consume the literal token

        Ok(self.push(Node::new(
            NodeKind::Expr {
                kind: ExprKind::Literal(literal_kind),
                type_: None,
            },
            literal_token.span,
        )))
    }

    fn parse_identifier_nud(&mut self, ident_token: Token) -> Result<Index, ParserError> {
        let name_symbol = match ident_token.kind {
            TokenKind::Identifier(s) => s,
            _ => {
                return Err(ParserError::InternalError("Not an identifier token in parse_identifier_nud".into()));
            }
        };

        self.advance(); // consume the identifier token

        // Check if this is a function call
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

    /// Parses `let name [: type] = value` as an expression (NUD for 'let' keyword)
    fn parse_let_expr_nud(&mut self, let_keyword_token: Token) -> Result<Index, ParserError> {
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

        // The value is an expression, parse it with full precedence.
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

    fn parse_return_expr_nud(&mut self, return_keyword_token: Token) -> Result<Index, ParserError> {
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

    fn parse_break_expr_nud(&mut self, break_keyword_token: Token) -> Result<Index, ParserError> {
        self.advance();

        Ok(self.push(Node::new(
            NodeKind::Expr {
                kind: ExprKind::Break,
                type_: None,
            },
            break_keyword_token.span,
        )))
    }

    fn parse_loop_expr_nud(&mut self, loop_keyword_token: Token) -> Result<Index, ParserError> {
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

    fn parse_paren_expr_nud(&mut self, open_paren_token: Token) -> Result<Index, ParserError> {
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
        actual_inner_node.span = full_span; // Update span to include parentheses
        Ok(inner_expr_idx)
    }

    fn parse_prefix_op_nud(&mut self, _op_token: Token) -> Result<Index, ParserError> {
        todo!()
    }

    fn parse_block_expr_nud(&mut self, _open_brace_token: Token) -> Result<Index, ParserError> {
        self.parse_block_body()
    }

    fn parse_if_expr_nud(&mut self, _if_token: Token) -> Result<Index, ParserError> {
        todo!()
    }

    fn parse_binary_infix_op_led(
        &mut self,
        op_token: Token,
        left_idx: Index,
        op_bp: BindingPower,
        is_right_assoc: bool,
    ) -> Result<Index, ParserError> {
        let op_kind = match op_token.kind {
            TokenKind::Punctuation(Punctuation::Plus) => BinOpKind::Add,
            TokenKind::Punctuation(Punctuation::Minus) => BinOpKind::Sub,
            TokenKind::Punctuation(Punctuation::Star) => BinOpKind::Mul,
            TokenKind::Punctuation(Punctuation::Slash) => BinOpKind::Div,
            TokenKind::Punctuation(Punctuation::Percent) => BinOpKind::Mod,
            TokenKind::Punctuation(Punctuation::EqEq) => BinOpKind::Eq,
            TokenKind::Punctuation(Punctuation::NotEq) => BinOpKind::NotEq,
            TokenKind::Punctuation(Punctuation::LessThan) => BinOpKind::LessThan,
            TokenKind::Punctuation(Punctuation::LessThanOrEq) => BinOpKind::LessThanOrEq,
            TokenKind::Punctuation(Punctuation::GreaterThan) => BinOpKind::GreaterThan,
            TokenKind::Punctuation(Punctuation::GreaterThanOrEq) => BinOpKind::GreaterThanOrEq,
            TokenKind::Punctuation(Punctuation::AmpAmp) => BinOpKind::LogicalAnd,
            TokenKind::Punctuation(Punctuation::PipePipe) => BinOpKind::LogicalOr,
            _ => {
                return Err(ParserError::InternalError(format!(
                    "Not a binary infix operator: {:?}",
                    op_token.kind
                )));
            }
        };

        let right_bp = if is_right_assoc { op_bp } else { op_bp + BindingPower::from(1) }; // Next level for left-assoc
        let right_idx = self.pratt_parse_expression(right_bp)?;

        let left_node_cloned = self
            .node(&left_idx)
            .ok_or_else(|| ParserError::InternalError("Left operand node not found".to_string()))?
            .clone();
        let right_node_cloned = self
            .node(&right_idx)
            .ok_or_else(|| ParserError::InternalError("Right operand node not found".to_string()))?
            .clone();
        let combined_span = left_node_cloned.span.connect_new(&right_node_cloned.span);

        Ok(self.push(Node::new(
            NodeKind::Expr {
                kind: ExprKind::BinOp {
                    left: left_idx,
                    right: right_idx,
                    op: op_kind,
                },
                type_: None,
            },
            combined_span,
        )))
    }

    fn parse_assignment_led(
        &mut self,
        _eq_token: Token,
        target_idx: Index,
        _is_right_assoc: bool, /* should be true */
    ) -> Result<Index, ParserError> {
        // L-value check (target must be assignable)
        let target_node_cloned = self
            .node(&target_idx)
            .ok_or_else(|| ParserError::InternalError("Target node not found for assignment".to_string()))?
            .clone();
        match target_node_cloned.kind {
            NodeKind::Expr {
                kind: ExprKind::Identifier(_),
                ..
            } => { /* OK */ }
            // TODO: Add other valid L-values (field access, array index)
            _ => {
                return Err(ParserError::InvalidLHSInAssignment {
                    span: target_node_cloned.span.to_display(&self.interner),
                });
            }
        }

        // for right-associativity, parse rhs with the same binding power.
        let value_idx = self.pratt_parse_expression(BindingPower::Assignment)?;

        let value_node_cloned = self
            .node(&value_idx)
            .ok_or_else(|| ParserError::InternalError("Value node not found for assignment".to_string()))?;
        let assignment_span = target_node_cloned.span.connect_new(&value_node_cloned.span);

        Ok(self.push(Node::new(
            NodeKind::Expr {
                kind: ExprKind::Assign {
                    target: target_idx,
                    value: value_idx,
                },
                type_: None,
            },
            assignment_span,
        )))
    }

    fn parse_function_call(&mut self, ident_token: Token, name_symbol: string_interner::symbol::SymbolUsize) -> Result<Index, ParserError> {
        let mut call_span = ident_token.span;

        // We know current token is '('
        self.advance(); // consume '('

        let mut args = Vec::new();

        // Handle empty argument list
        if self.current().kind == TokenKind::Punctuation(Punctuation::CloseParen) {
            let close_paren_token = *self.current();
            call_span.connect_mut(&close_paren_token.span);
            self.advance(); // consume ')'
        } else {
            // Parse arguments
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
                        return Err(ParserError::CloseParenExpected { token: *self.current() });
                    }
                }
            }
        }

        // Create function identifier node
        let func_node = Node::new(
            NodeKind::Expr {
                kind: ExprKind::Identifier(name_symbol),
                type_: None,
            },
            ident_token.span,
        );
        let func_idx = self.push(func_node);

        // Create function call node
        Ok(self.push(Node::new(
            NodeKind::Expr {
                kind: ExprKind::Call { func: func_idx, args },
                type_: None,
            },
            call_span,
        )))
    }

    fn parse_call_led(&mut self, _open_paren_token: Token, func_idx: Index) -> Result<Index, ParserError> {
        let func_node = self
            .node(&func_idx)
            .ok_or_else(|| ParserError::InternalError("Function node not found for call".to_string()))?
            .clone();
        let mut call_span = func_node.span;

        let mut args = Vec::new();

        // Handle empty argument list
        if self.current().kind == TokenKind::Punctuation(Punctuation::CloseParen) {
            let close_paren_token = *self.current();
            call_span.connect_mut(&close_paren_token.span);
            self.advance(); // consume ')'
        } else {
            // Parse arguments
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
                        return Err(ParserError::CloseParenExpected { token: *self.current() });
                    }
                }
            }
        }

        // Create function call node
        Ok(self.push(Node::new(
            NodeKind::Expr {
                kind: ExprKind::Call { func: func_idx, args },
                type_: None,
            },
            call_span,
        )))
    }
    // TODO: fn parse_index_led(&mut self, open_bracket_token: Token, array_idx: Index) -> Result<Index, ParserError>
    // TODO: fn parse_field_access_led(&mut self, dot_token: Token, object_idx: Index) -> Result<Index, ParserError>
}

impl From<u32> for BindingPower {
    fn from(val: u32) -> Self {
        match val {
            0 => BindingPower::None,
            1 => BindingPower::Assignment,
            2 => BindingPower::LogicalOr,
            3 => BindingPower::LogicalAnd,
            4 => BindingPower::Equality,
            5 => BindingPower::Comparison,
            6 => BindingPower::Term,
            7 => BindingPower::Factor,
            8 => BindingPower::Unary,
            9 => BindingPower::Call,
            10 => BindingPower::MemberAccess,
            11 => BindingPower::Primary,
            _ => BindingPower::More(val),
        }
    }
}

impl BindingPower {
    fn val(&self) -> u32 {
        match self {
            BindingPower::None => 0,
            BindingPower::Assignment => 1,
            BindingPower::LogicalOr => 2,
            BindingPower::LogicalAnd => 3,
            BindingPower::Equality => 4,
            BindingPower::Comparison => 5,
            BindingPower::Term => 6,
            BindingPower::Factor => 7,
            BindingPower::Unary => 8,
            BindingPower::Call => 9,
            BindingPower::MemberAccess => 10,
            BindingPower::Primary => 11,
            BindingPower::More(val) => *val,
        }
    }
}

impl Add for BindingPower {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        let sum_val = self.val() + rhs.val();
        BindingPower::from(sum_val)
    }
}
