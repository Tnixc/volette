mod error;
mod type_check;

use generational_arena::Arena;
use string_interner::{StringInterner, backend::BucketBackend, symbol::SymbolUsize};

use crate::compiler::parser::node::Node;

pub fn analysis_pass(root: &Node, interner: &StringInterner<BucketBackend<SymbolUsize>>, mut nodes: &mut Arena<Node>) {
    type_check::type_check_root(root, &interner, &mut nodes);
}
