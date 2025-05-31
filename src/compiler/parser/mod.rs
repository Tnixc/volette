use error::ParserError;
use generational_arena::{Arena, Index};
use node::{Def, DefKind, Node, NodeKind};
use string_interner::{backend::BucketBackend, symbol::SymbolUsize, StringInterner};

use super::tokens::{Token, TokenKind};

mod block;
mod calls;
mod definitions;
mod error;
mod expr;
mod node;
mod statements;

pub struct Parser {
    tree: Arena<Node>,
    interner: StringInterner<BucketBackend<SymbolUsize>>,
    tokens: Vec<Token>,
    current_idx: usize,
    current_token: Token,
    parse_errors: Vec<ParserError>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>, interner: StringInterner<BucketBackend<SymbolUsize>>) -> Self {
        Self {
            tree: Arena::with_capacity(tokens.len()),
            interner,
            current_idx: 0,
            current_token: *tokens.first().unwrap(), // FIXME in lexer -> parser stage
            tokens,
            parse_errors: Vec::new(),
        }
    }

    pub fn parse(&mut self) {
        let root = self.parse_root();
        println!("errors: {:?}", self.parse_errors);
        self.tree.insert(root);
        println!("tree: {:?}", self.tree);
    }

    pub fn parse_root(&mut self) -> Node {
        let mut span = self.current().span;
        let mut defs = Vec::new();

        while self.advance().is_some_and(|t| t.kind != TokenKind::Eof) {
            match self.parse_def() {
                Ok(idx) => defs.push(idx),
                Err(e) => self.parse_errors.push(e),
            }
        }

        if let Some(last_node) = defs.last().and_then(|idx| self.node(idx)) {
            span.connect_mut(&last_node.span);
        }

        Node {
            kind: NodeKind::Root { defs },
            span,
            type_: None,
        }
    }

    pub fn advance_unchecked(&mut self) -> Token {
        self.current_idx += 1;
        self.current_token = *self
            .tokens
            .get(self.current_idx)
            .expect("No next token but advanced anyway");
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
        self.current_token = *self
            .tokens
            .get(self.current_idx)
            .expect("No previous token but backtracked anyway");
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
