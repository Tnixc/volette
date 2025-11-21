use generational_arena::{Arena, Index};
use node::{Node, NodeKind};
use rootcause::prelude::*;
use string_interner::{StringInterner, backend::BucketBackend, symbol::SymbolUsize};

use super::tokens::{Keyword, Token, TokenKind};
use crate::{SafeConvert, compiler::tokens::Punctuation};

pub mod as_;
pub mod assignment;
pub mod binary_ops;
pub mod block;
pub mod control_flow;
pub mod definitions;
pub mod desugar;
pub mod display;
// pub mod error;
pub mod expr;
pub mod identifier;
pub mod literal;
#[macro_use]
pub mod node;
pub mod paren_block;
pub mod precedence;
pub mod unary_ops;

pub struct Parser<'a> {
    pub tree: Arena<Node>,
    pub interner: &'a mut StringInterner<BucketBackend<SymbolUsize>>,
    pub tokens: Vec<Token>,
    pub current_idx: usize,
    pub current_token: Token,
    pub parse_errors: Vec<Report>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: Vec<Token>, interner: &'a mut StringInterner<BucketBackend<SymbolUsize>>) -> Self {
        Self {
            tree: Arena::with_capacity(tokens.len()),
            interner,
            current_idx: 0,
            current_token: *tokens.first().safe(),
            tokens,
            parse_errors: Vec::new(),
        }
    }

    pub fn parse(&mut self) -> Node {
        let root = self.parse_root();
        // println!("errors: {:?}", self.parse_errors);
        // root.print_tree(&self.tree, self.interner);
        root
    }

    pub fn parse_root(&mut self) -> Node {
        let mut span = self.current().span;
        let mut defs = Vec::new();

        while self.advance().is_some_and(|t| t.kind != TokenKind::Eof) {
            match self.parse_def() {
                Ok(idx) => defs.push(idx),
                Err(e) => {
                    self.parse_errors.push(e);
                    // skip to the next potential "stable" section to avoid cascading errors
                    // while self.advance().is_some_and(|t| t.kind != TokenKind::Eof && t.kind != TokenKind::Keyword(Keyword::Fn)) {}
                    while self.advance().is_some_and(|t| {
                        !matches!(
                            t.kind,
                            TokenKind::Eof
                                | TokenKind::Punctuation(Punctuation::OpenParen)
                                | TokenKind::Punctuation(Punctuation::OpenBrace)
                                | TokenKind::Punctuation(Punctuation::Semicolon)
                                | TokenKind::Keyword(Keyword::Fn)
                        )
                    }) {}
                }
            }
        }

        if let Some(last_node) = defs.last().and_then(|idx| self.node(idx)) {
            span.connect_mut(&last_node.span);
        }

        Node {
            kind: NodeKind::Root { defs },
            span,
        }
    }

    pub fn advance_unchecked(&mut self) -> Token {
        self.current_idx += 1;
        self.current_token = *self.tokens.get(self.current_idx).safe();
        self.current_token
    }

    /// returns true if there is a next token, and advances if there is
    pub fn advance(&mut self) -> Option<Token> {
        if self.current_idx < self.tokens.len() - 1 {
            Some(self.advance_unchecked())
        } else {
            None
        }
    }

    pub fn backtrack_unchecked(&mut self) {
        self.current_idx -= 1;
        self.current_token = *self.tokens.get(self.current_idx).safe();
    }

    pub fn backtrack(&mut self) {
        if self.current_idx > 0 {
            self.backtrack_unchecked();
        }
    }

    pub fn peek_offset(&self, offset: usize) -> Option<&Token> {
        self.tokens.get(self.current_idx + offset)
    }

    /// looks at current token
    pub fn current(&self) -> &Token {
        &self.tokens[self.current_idx]
    }

    pub fn node(&self, idx: &Index) -> Option<&Node> {
        self.tree.get(*idx)
    }

    pub fn push(&mut self, node: Node) -> Index {
        self.tree.insert(node)
    }
}
