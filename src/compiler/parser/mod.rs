use error::ParserError;
use generational_arena::{Arena, Index};
use node::{DefKind, Node, NodeKind};
use string_interner::{backend::BucketBackend, symbol::SymbolUsize, StringInterner};

use super::tokens::{Token, TokenKind};

mod block;
mod definitions;
mod error;
mod node;

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
            tree: Arena::new(),
            interner,
            current_idx: 0,
            current_token: tokens.first().unwrap().clone(), // FIXME in lexer -> parser stage
            tokens,
            parse_errors: Vec::new(),
        }
    }

    pub fn parse(&mut self) {
        let root = self.parse_root();
        println!("errors: {:?}", self.parse_errors);
        match &root.kind {
            NodeKind::Root { defs } => {
                defs.iter().for_each(|idx| match &self.tree.get(*idx).unwrap().kind {
                    NodeKind::Def(DefKind::Function {
                        name,
                        params,
                        body,
                        return_type,
                    }) => {
                        println!("name: {:?}", self.interner.resolve(*name));
                        println!(
                            "params: {:?}",
                            params
                                .iter()
                                .map(|(name, ty)| (self.interner.resolve(*name), ty))
                                .collect::<Vec<_>>()
                        );
                        println!("body: {:?}", body);
                        println!("return_type: {:?}", return_type);
                    }
                    _ => {}
                });
            }
            _ => {}
        }
        self.tree.insert(root);
    }

    pub fn parse_root(&mut self) -> Node {
        let mut span = self.current().span;
        let mut defs = Vec::new();

        while self.current().kind != TokenKind::Eof {
            match self.parse_def() {
                Ok(idx) => defs.push(idx),
                Err(e) => self.parse_errors.push(e),
            }
            self.advance();
        }

        if let Some(last_node) = defs.last().and_then(|idx| self.node(&idx)) {
            span.connect_mut(&last_node.span);
        }

        Node {
            kind: NodeKind::Root { defs },
            span,
            type_: None,
        }
    }
    pub fn advance(&mut self) {
        self.current_idx += 1;
        self.current_token = self
            .tokens
            .get(self.current_idx)
            .expect("No next token but advanced anyway")
            .clone();
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
