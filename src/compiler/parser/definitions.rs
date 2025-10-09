use generational_arena::Index;
use string_interner::symbol::SymbolUsize;

use crate::{
    SafeConvert,
    compiler::{
        parser::node::Type,
        tokens::{Keyword, PrimitiveTypes, Punctuation, Span, TokenKind},
    },
};

use super::{
    Parser,
    error::ParserError,
    node::{DefKind, Node, NodeKind},
};

impl<'a> Parser<'a> {
    pub fn parse_def(&mut self) -> Result<Index, ParserError> {
        match self.current().kind {
            TokenKind::Keyword(Keyword::Fn) => self.parse_fn_def(),
            _ => {
                return Err(ParserError::Unexpected {
                    what: format!("token at top level: {:?}", self.current().kind),
                    context: "Only function and struct definitions are allowed at the top level".to_string(),
                    span: self.current().span.to_display(self.interner),
                });
            }
        }
    }

    fn parse_fn_def(&mut self) -> Result<Index, ParserError> {
        let start_span = self.current().span;
        self.advance(); // we know there's a fn keyword

        let name = match self.current().kind {
            TokenKind::Identifier(name) => name,
            _ => {
                return Err(ParserError::Expected {
                    what: "identifier after 'fn'".to_string(),
                    got: format!("{:?}", self.current().kind),
                    span: self.current().span.to_display(self.interner),
                });
            }
        };
        self.advance();

        match self.current().kind {
            TokenKind::Punctuation(Punctuation::OpenParen) => {} // it's a function
            _ => {
                return Err(ParserError::Expected {
                    what: "opening parenthesis after function name".to_string(),
                    got: format!("{:?}", self.current().kind),
                    span: self.current().span.to_display(self.interner),
                });
            }
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
                        mode = ParamMode::Colon;
                        param.0 = Some(name);
                        param.2 = Some(self.current().span);
                    }
                    _ => {
                        mode = ParamMode::Colon;
                        self.parse_errors.push(ParserError::Expected {
                            what: "parameter name".to_string(),
                            got: format!("{:?}", self.current().kind),
                            span: self.current().span.to_display(self.interner),
                        });
                        self.advance();
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
                        mode = ParamMode::Type;
                        self.parse_errors.push(ParserError::Expected {
                            what: "colon after parameter name".to_string(),
                            got: format!("{:?}", self.current().kind),
                            span: self.current().span.to_display(self.interner),
                        });
                        self.advance();
                    }
                },
                ParamMode::Type => match self.current().kind {
                    TokenKind::TypeLiteral(ty) => {
                        mode = ParamMode::Comma;
                        if let Some(span) = param.2.as_mut() {
                            span.connect_mut(&self.current().span);
                        }
                        params.push((param.0.safe(), Type::Primitive(ty), param.2.safe()));
                        param = (None, None, None);
                    }
                    _ => {
                        mode = ParamMode::Comma;
                        self.parse_errors.push(ParserError::Expected {
                            what: "type annotation".to_string(),
                            got: format!("{:?}", self.current().kind),
                            span: self.current().span.to_display(self.interner),
                        });
                        self.advance();
                    }
                },
                ParamMode::Comma => match self.current().kind {
                    TokenKind::Punctuation(Punctuation::Comma) => {
                        mode = ParamMode::Name;
                    }
                    _ => {
                        mode = ParamMode::Name;
                        self.parse_errors.push(ParserError::Expected {
                            what: "comma between parameters".to_string(),
                            got: format!("{:?}", self.current().kind),
                            span: self.current().span.to_display(self.interner),
                        });
                        self.advance();
                    }
                },
            }
        }

        match self.current().kind {
            TokenKind::Punctuation(Punctuation::CloseParen) => {
                if mode != ParamMode::Comma && !params.is_empty() {
                    self.parse_errors.push(ParserError::Invalid {
                        what: "function parameter".to_string(),
                        reason: "Parameter must have both a name and a type".to_string(),
                        span: self.current().span.to_display(self.interner),
                    });
                }
            }
            _ => {
                return Err(ParserError::Expected {
                    what: "closing parenthesis".to_string(),
                    got: format!("{:?}", self.current().kind),
                    span: self.current().span.to_display(self.interner),
                });
            }
        };

        self.advance();

        let mut return_type = Type::Primitive(PrimitiveTypes::Nil);

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
                _ => {
                    return Err(ParserError::Expected {
                        what: "return type after ':'".to_string(),
                        got: format!("{:?}", self.current().kind),
                        span: self.current().span.to_display(self.interner),
                    });
                }
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
            _ => {
                return Err(ParserError::Expected {
                    what: "function body (opening brace)".to_string(),
                    got: format!("{:?}", self.current().kind),
                    span: self.current().span.to_display(self.interner),
                });
            }
        }
    }
}
