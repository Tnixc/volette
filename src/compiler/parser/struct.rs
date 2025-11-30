use generational_arena::Index;
use rootcause::Report;
use string_interner::symbol::SymbolUsize;

use crate::{
    compiler::{
        parser::{
            Parser,
            node::{DefKind, Node, NodeKind, VType},
        },
        tokens::{Punctuation, Span, TokenKind},
    },
    parse_err,
};

#[derive(Debug, Clone, PartialEq)]
pub struct Field {
    pub name: SymbolUsize,
    pub ty: VType,
    pub span: Span,
}

impl<'a> Parser<'a> {
    pub fn parse_struct(&mut self) -> Result<Index, Report> {
        let mut start_span = self.current().span;
        self.advance(); // we know there's a 'struct' keyword
        if let TokenKind::Identifier(name) = self.current().kind {
            self.advance(); // consume identifier

            let struct_def = self.parse_struct_body()?;
            return Ok(self.push(Node {
                kind: NodeKind::Def {
                    kind: DefKind::Struct { name, fields: struct_def },
                },
                span: *start_span.connect_mut(&self.current().span),
            }));
        } else {
            return Err(parse_err!(
                "Expected identifier for struct definition",
                Some(self.current().span.to_display(self.interner)),
            ));
        }
    }

    pub fn parse_struct_body(&mut self) -> Result<VType, Report> {
        self.advance(); // consume '{'

        let mut fields: Vec<Field> = vec![];

        while self.current().kind != TokenKind::Punctuation(Punctuation::CloseBrace) {
            fields.push(self.parse_struct_field()?);

            if self.current().kind != TokenKind::Punctuation(Punctuation::Comma) {
                return Err(parse_err!(
                    "Expected ',' after struct field type definition",
                    Some(self.current().span.to_display(self.interner)),
                ));
            }
            self.advance(); // consume ','
        }
        return Ok(VType::Struct(fields));
    }

    fn parse_struct_field(&mut self) -> Result<Field, Report> {
        let mut start_span = self.current().span;
        if let TokenKind::Identifier(name) = self.current().kind {
            self.advance(); // consume identifier

            if self.current().kind != TokenKind::Punctuation(Punctuation::Colon) {
                return Err(parse_err!(
                    "Expected ':' after struct field name",
                    Some(self.current().span.to_display(self.interner)),
                ));
            }

            self.advance(); // consume ':'

            let ty = self.parse_type()?;

            return Ok(Field {
                name,
                ty: ty.0,
                span: *start_span.connect_mut(&ty.1),
            });
        } else {
            return Err(parse_err!(
                "Expected identifier for struct field name",
                Some(self.current().span.to_display(self.interner)),
            ));
        }
    }
}
