use generational_arena::{Arena, Index};
use node::Node;

mod node;

pub enum ParserState {
    TopLevel,
    FunctionDef,
    StructDef,
}

pub struct Parser {
    tree: Arena<Node>,
    top_level: Vec<Index>,
    parser_state: ParserState,
}

impl Parser {
    pub fn new() -> Self {
        Self {
            tree: Arena::new(),
            top_level: Vec::new(),
            parser_state: ParserState::TopLevel,
        }
    }
}
