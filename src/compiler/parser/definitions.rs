use generational_arena::Index;
use string_interner::symbol::SymbolUsize;

use crate::compiler::{
    parser::node::Type,
    tokens::{Keyword, PrimitiveTypes, Punctuation, TokenKind},
};

use super::{
    error::ParserError,
    node::{DefKind, Node, NodeKind},
    Parser,
};

impl Parser {
    pub fn parse_def(&mut self) -> Result<Index, ParserError> {
        match self.current().kind {
            TokenKind::Keyword(Keyword::Fn) => self.parse_fn_def(),
            _ => {
                return Err(ParserError::UnexpectedTokenAtTopLevel {
                    token: self.current().clone(),
                })
            }
        }
    }

    fn parse_fn_def(&mut self) -> Result<Index, ParserError> {
        let start_span = self.current().span;
        self.advance(); // we know there's a fn keyword

        let name = match self.current().kind {
            TokenKind::Identifier(name) => name,
            _ => {
                return Err(ParserError::IdentifierExpectedAfterFn {
                    token: self.current().clone(),
                })
            }
        };
        self.advance();

        match self.current().kind {
            TokenKind::Punctuation(Punctuation::OpenParen) => {}
            _ => {
                return Err(ParserError::OpenParenExpectedAfterFnName {
                    token: self.current().clone(),
                })
            }
        };
        self.advance();

        let mut param: (Option<SymbolUsize>, Option<Type>) = (None, None);
        let mut params: Vec<(SymbolUsize, Type)> = Vec::new();

        enum ParamMode {
            Name,
            Colon,
            Type,
            Comma,
        }
        let mut mode = ParamMode::Name;

        while self.current().kind != TokenKind::Punctuation(Punctuation::CloseParen) {
            match mode {
                ParamMode::Name => match self.current().kind {
                    TokenKind::Identifier(name) => {
                        self.advance();
                        param.0 = Some(name);
                        mode = ParamMode::Colon;
                    }
                    _ => {
                        println!("name: {:?}", self.current());
                        return Err(ParserError::FnParamNameExpected {
                            token: self.current().clone(),
                        });
                    }
                },
                ParamMode::Colon => match self.current().kind {
                    TokenKind::Punctuation(Punctuation::Colon) => {
                        self.advance();
                        mode = ParamMode::Type;
                    }
                    _ => {
                        return Err(ParserError::FnParamTypeExpected {
                            token: self.current().clone(),
                        })
                    }
                },
                ParamMode::Type => match self.current().kind {
                    TokenKind::TypeLiteral(ty) => {
                        self.advance();
                        params.push((param.0.expect("[!] name should be Some"), Type::Primitive(ty)));
                        param = (None, None);
                        mode = ParamMode::Comma;
                    }
                    _ => {
                        return Err(ParserError::FnParamTypeExpected {
                            token: self.current().clone(),
                        })
                    }
                },
                ParamMode::Comma => match self.current().kind {
                    TokenKind::Punctuation(Punctuation::Comma) => {
                        self.advance();
                        mode = ParamMode::Name;
                    }
                    _ => {
                        return Err(ParserError::FnParamCommaExpected {
                            token: self.current().clone(),
                        })
                    }
                },
            }
        }

        match self.current().kind {
            TokenKind::Punctuation(Punctuation::CloseParen) => {}
            _ => {
                return Err(ParserError::CloseParenExpected {
                    token: self.current().clone(),
                })
            }
        };
        self.advance();

        let mut return_type = Type::Primitive(PrimitiveTypes::Nil);

        match self.current().kind {
            TokenKind::Punctuation(Punctuation::Colon) => {
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
                        return Err(ParserError::FnReturnTypeExpected {
                            token: self.current().clone(),
                        })
                    }
                }
            }
            _ => {}
        }

        match self.current().kind {
            TokenKind::Punctuation(Punctuation::OpenBrace) => {
                let body = self.parse_block()?;
                let node = Node::new(
                    NodeKind::Def(DefKind::Function {
                        name,
                        params,
                        body,
                        return_type,
                    }),
                    start_span.connect_new(&self.current().span),
                );
                Ok(self.push(node))
            }
            _ => {
                return Err(ParserError::FnBodyExpected {
                    token: self.current().clone(),
                })
            }
        }
    }
}
