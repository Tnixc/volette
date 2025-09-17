use std::collections::HashMap;

use generational_arena::Arena;
use string_interner::{StringInterner, backend::BucketBackend, symbol::SymbolUsize};

use crate::compiler::parser::node::{DefKind, Node, NodeKind, Type};

pub fn generate_function_table(nodes: &mut Arena<Node>) -> HashMap<SymbolUsize, (Box<Vec<Type>>, Type)> {
    let mut function_table = HashMap::new();
    nodes.iter().for_each(|node| match &node.1.kind {
        NodeKind::Def { kind } => match kind {
            DefKind::Function {
                name, params, return_type, ..
            } => {
                function_table.insert(*name, (Box::new(params.iter().map(|param| param.1).collect()), *return_type));
            }
            _ => (),
        },
        _ => (),
    });

    return function_table;
}
