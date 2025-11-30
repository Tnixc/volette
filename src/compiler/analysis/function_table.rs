use std::collections::HashMap;

use generational_arena::Arena;
use string_interner::symbol::SymbolUsize;

use crate::compiler::parser::node::{DefKind, Node, NodeKind, VType};

pub fn generate_function_table(nodes: &mut Arena<Node>) -> HashMap<SymbolUsize, (Vec<VType>, VType)> {
    let mut function_table = HashMap::new();
    nodes.iter().for_each(|node| match &node.1.kind {
        NodeKind::Def { kind } => match kind {
            DefKind::Function {
                name, params, return_type, ..
            } => {
                function_table.insert(*name, (params.iter().map(|param| param.1.clone()).collect(), return_type.clone()));
            }
            _ => (),
        },
        _ => (),
    });

    return function_table;
}
