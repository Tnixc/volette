mod error;
mod type_check;

use generational_arena::Arena;
use string_interner::{backend::BucketBackend, symbol::SymbolUsize, StringInterner};

use crate::compiler::parser::node::Node;

pub fn analysis_pass(root: &Node, interner: StringInterner<BucketBackend<SymbolUsize>>, nodes: Arena<Node>) {
    type_check::type_check_root(root, interner, nodes);
}
