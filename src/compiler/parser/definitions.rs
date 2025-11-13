use generational_arena::Index;
use string_interner::symbol::SymbolUsize;

use crate::{
    SafeConvert,
    compiler::{
        parser::node::VType,
        tokens::{Keyword, PrimitiveTypes, Punctuation, Span, TokenKind},
    },
};

use super::{
    Parser,
    error::ParserError,
    node::{DefKind, Node, NodeKind},
};

impl<'a> Parser<'a> {
    /// Parse a type annotation and return both the type and its span.
    /// Advances the cursor past the type.
    pub fn parse_type(&mut self) -> Result<(VType, Span), ParserError> {
        let start_span = self.current().span;

        // count leading carets (^) for pointer depth
        let mut pointer_depth = 0;
        while let TokenKind::Punctuation(Punctuation::Caret) = self.current().kind {
            pointer_depth += 1;
            self.advance();
        }

        let mut base_type = match self.current().kind {
            TokenKind::TypeLiteral(ty) => VType::Primitive(ty),
            TokenKind::Identifier(name) => VType::Custom(name),
            _ => {
                return Err(ParserError::Expected {
                    what: "type (primitive or identifier)".to_string(),
                    got: format!("{:?}", self.current().kind),
                    span: self.current().span.to_display(self.interner),
                });
            }
        };

        let type_span = start_span.connect_new(&self.current().span);

        for _ in 0..pointer_depth {
            base_type = VType::Pointer(Box::new(base_type));
        }

        // advance past the base type
        self.advance();

        Ok((base_type, type_span))
    }

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

        let mut param: (Option<SymbolUsize>, Option<VType>, Option<Span>) = (None, None, None);
        let mut params: Vec<(SymbolUsize, VType, Span)> = Vec::new();

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
                ParamMode::Type => {
                    match self.parse_type() {
                        Ok((ty, type_span)) => {
                            mode = ParamMode::Comma;
                            // backtrack because parse_type advanced past the type,
                            // but the loop will advance again
                            // this is actually so dumb
                            self.backtrack();
                            if let Some(span) = param.2.as_mut() {
                                span.connect_mut(&type_span);
                            }
                            params.push((param.0.safe(), ty, param.2.safe()));
                            param = (None, None, None);
                        }
                        Err(e) => {
                            mode = ParamMode::Comma;
                            self.parse_errors.push(e);
                        }
                    }
                }
                ParamMode::Comma => match self.current().kind {
                    TokenKind::Punctuation(Punctuation::Comma) => {
                        mode = ParamMode::Name;
                    }
                    TokenKind::Punctuation(Punctuation::CloseParen) => {
                        break;
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

        let mut return_type = VType::Primitive(PrimitiveTypes::Nil);

        if let TokenKind::Punctuation(Punctuation::Colon) = self.current().kind {
            self.advance();
            let (parsed_type, _) = self.parse_type()?;
            return_type = parsed_type;
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
