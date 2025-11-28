use generational_arena::Index;
use string_interner::symbol::SymbolUsize;

use crate::{
    SafeConvert,
    compiler::{
        error::Help,
        parser::node::VType,
        tokens::{Keyword, PrimitiveTypes, Punctuation, Span, TokenKind},
    },
};
use rootcause::prelude::*;

use super::{
    Parser,
    node::{DefKind, Node, NodeKind},
};

impl<'a> Parser<'a> {
    /// Parse a type annotation and return both the type and its span.
    /// Advances the cursor past the type.
    pub fn parse_type(&mut self) -> Result<(VType, Span), Report> {
        let start_span = self.current().span;

        // count leading amps (&) for pointer depth
        let mut pointer_depth = 0;
        while let TokenKind::Punctuation(Punctuation::Amp) = self.current().kind {
            pointer_depth += 1;
            self.advance();
        }

        let mut base_type = match self.current().kind {
            TokenKind::TypeLiteral(ty) => VType::Primitive(ty),
            TokenKind::Identifier(name) => VType::Custom(name),
            _ => {
                return Err(crate::parse_err!(
                    "Expected type (primitive or identifier), got {:?}",
                    Some(self.current().span.to_display(self.interner)),
                    self.current().kind
                )
                .attach(Help("Check the syntax - the parser expected something different here".into())));
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

    pub fn parse_def(&mut self) -> Result<Index, Report> {
        match self.current().kind {
            TokenKind::Keyword(Keyword::Fn) => self.parse_fn_def(),
            _ => {
                return Err(crate::parse_err!(
                    "Unexpected token at top level: {:?}",
                    Some(self.current().span.to_display(self.interner)),
                    self.current().kind
                )
                .attach(Help("Only function and struct definitions are allowed at the top level".into())));
            }
        }
    }

    fn parse_fn_def(&mut self) -> Result<Index, Report> {
        let start_span = self.current().span;
        self.advance(); // we know there's a fn keyword

        let name = match self.current().kind {
            TokenKind::Identifier(name) => name,
            _ => {
                return Err(crate::parse_err!(
                    "Expected identifier after 'fn', got {:?}",
                    Some(self.current().span.to_display(self.interner)),
                    self.current().kind
                )
                .attach(Help("Check the syntax - the parser expected something different here".into())));
            }
        };
        self.advance();

        match self.current().kind {
            TokenKind::Punctuation(Punctuation::OpenParen) => {} // it's a function
            _ => {
                return Err(crate::parse_err!(
                    "Expected opening parenthesis after function name, got {:?}",
                    Some(self.current().span.to_display(self.interner)),
                    self.current().kind
                )
                .attach(Help("Check the syntax - the parser expected something different here".into())));
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
                        self.parse_errors.push(
                            crate::parse_err!(
                                "Expected parameter name, got {:?}",
                                Some(self.current().span.to_display(self.interner)),
                                self.current().kind
                            )
                            .attach(Help("Check the syntax - the parser expected something different here".into())),
                        );
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
                        self.parse_errors.push(
                            crate::parse_err!(
                                "Expected colon after parameter name, got {:?}",
                                Some(self.current().span.to_display(self.interner)),
                                self.current().kind
                            )
                            .attach(Help("Check the syntax - the parser expected something different here".into())),
                        );
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
                        self.parse_errors.push(
                            crate::parse_err!(
                                "Expected comma between parameters, got {:?}",
                                Some(self.current().span.to_display(self.interner)),
                                self.current().kind
                            )
                            .attach(Help("Check the syntax - the parser expected something different here".into())),
                        );
                        self.advance();
                    }
                },
            }
        }

        match self.current().kind {
            TokenKind::Punctuation(Punctuation::CloseParen) => {
                if mode != ParamMode::Comma && !params.is_empty() {
                    self.parse_errors.push(
                        crate::parse_err!(
                            "Invalid function parameter: Parameter must have both a name and a type",
                            Some(self.current().span.to_display(self.interner))
                        )
                        .attach(Help("This construct is not valid in the current context".into())),
                    );
                }
            }
            _ => {
                return Err(crate::parse_err!(
                    "Expected closing parenthesis, got {:?}",
                    Some(self.current().span.to_display(self.interner)),
                    self.current().kind
                )
                .attach(Help("Check the syntax - the parser expected something different here".into())));
            }
        };

        self.advance();

        let mut return_type = VType::Primitive(PrimitiveTypes::Unit);

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
                return Err(crate::parse_err!(
                    "Expected function body (opening brace), got {:?}",
                    Some(self.current().span.to_display(self.interner)),
                    self.current().kind
                )
                .attach(Help("Check the syntax - the parser expected something different here".into())));
            }
        }
    }
}
