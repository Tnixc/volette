use generational_arena::Index;
use string_interner::symbol::SymbolUsize;

use crate::compiler::{
    parser::node::Type,
    tokens::{Keyword, PrimitiveTypes, Punctuation, Span, TokenKind},
};

use super::{
    Parser,
    error::ParserError,
    node::{DefKind, Node, NodeKind},
};

impl Parser {
    pub fn parse_def(&mut self) -> Result<Index, ParserError> {
        match self.current().kind {
            TokenKind::Keyword(Keyword::Fn) => self.parse_fn_def(),
            _ => return Err(ParserError::UnexpectedTokenAtTopLevel { token: *self.current() }),
        }
    }

    fn parse_fn_def(&mut self) -> Result<Index, ParserError> {
        let start_span = self.current().span;
        self.advance(); // we know there's a fn keyword

        let name = match self.current().kind {
            TokenKind::Identifier(name) => name,
            _ => return Err(ParserError::IdentifierExpectedAfterFn { token: *self.current() }),
        };
        self.advance();

        match self.current().kind {
            TokenKind::Punctuation(Punctuation::OpenParen) => {}
            _ => return Err(ParserError::OpenParenExpectedAfterFnName { token: *self.current() }),
        };

        let mut param: (Option<SymbolUsize>, Option<Type>, Option<Span>) = (None, None, None);
        let mut params: Vec<(SymbolUsize, Type, Span)> = Vec::new();

        #[derive(Debug, PartialEq, Eq)]
        enum ParamMode {
            Name,
            Colon,
            Type,
            Comma,
        }
        let mut mode = ParamMode::Name;

        while self
            .advance()
            .is_some_and(|t| t.kind != TokenKind::Punctuation(Punctuation::CloseParen))
        {
            match mode {
                ParamMode::Name => match self.current().kind {
                    TokenKind::Identifier(name) => {
                        param.0 = Some(name);
                        param.2 = Some(self.current().span);
                        mode = ParamMode::Colon;
                    }
                    _ => {
                        self.parse_errors.push(ParserError::FnParamNameExpected { token: *self.current() });
                        mode = ParamMode::Colon;
                        self.backtrack();
                    }
                },
                ParamMode::Colon => match self.current().kind {
                    TokenKind::Punctuation(Punctuation::Colon) => {
                        mode = ParamMode::Type;
                        if let Some(span) = param.2.as_mut() {
                            span.connect_mut(&self.current().span);
                        }
                    }
                    _ => {
                        self.parse_errors.push(ParserError::FnParamTypeExpected { token: *self.current() });
                        mode = ParamMode::Type;
                        self.backtrack();
                    }
                },
                ParamMode::Type => match self.current().kind {
                    TokenKind::TypeLiteral(ty) => {
                        if let Some(span) = param.2.as_mut() {
                            span.connect_mut(&self.current().span);
                        }
                        params.push((
                            param.0.expect("[!] name should be Some"),
                            Type::Primitive(ty),
                            param.2.expect("[!] span should be Some"),
                        ));
                        param = (None, None, None);
                        mode = ParamMode::Comma;
                    }
                    _ => {
                        self.parse_errors.push(ParserError::FnParamTypeExpected { token: *self.current() });
                        mode = ParamMode::Type;
                        self.backtrack();
                    }
                },
                ParamMode::Comma => match self.current().kind {
                    TokenKind::Punctuation(Punctuation::Comma) => {
                        mode = ParamMode::Name;
                    }
                    _ => {
                        self.parse_errors.push(ParserError::FnParamCommaExpected { token: *self.current() });
                        mode = ParamMode::Name;
                        self.backtrack();
                    }
                },
            }
        }
        match self.current().kind {
            TokenKind::Punctuation(Punctuation::CloseParen) => {
                if mode != ParamMode::Comma && !params.is_empty() {
                    self.parse_errors
                        .push(ParserError::FnParameterIncomplete { token: *self.current() });
                }
            }
            _ => return Err(ParserError::CloseParenExpected { token: *self.current() }),
        };

        self.advance();

        let mut return_type = Type::Primitive(PrimitiveTypes::Unit);

        if let TokenKind::Punctuation(Punctuation::Colon) = self.current().kind {
            self.advance();
            match self.current().kind {
                TokenKind::TypeLiteral(ty) => {
                    return_type = Type::Primitive(ty);
                    self.advance();
                }
                TokenKind::Identifier(name) => {
                    return_type = Type::Custom(name);
                    self.advance();
                }
                _ => return Err(ParserError::FnReturnTypeExpected { token: *self.current() }),
            }
        }

        match self.current().kind {
            TokenKind::Punctuation(Punctuation::OpenBrace) => {
                let body = self.parse_block_body()?;
                self.backtrack();
                let node = Node::new(
                    NodeKind::Def {
                        kind: DefKind::Function {
                            name,
                            params,
                            body,
                            return_type,
                        },
                    },
                    start_span.connect_new(&self.current().span),
                );
                Ok(self.push(node))
            }
            _ => return Err(ParserError::FnBodyExpected { token: *self.current() }),
        }
    }
}
