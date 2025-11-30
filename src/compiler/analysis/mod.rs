mod checks;
mod compatible;
mod context;
mod function_table;
mod resolve;
mod type_check;

pub use context::TypeCtx;

use std::collections::HashMap;

use generational_arena::Arena;
use string_interner::{StringInterner, backend::BucketBackend, symbol::SymbolUsize};

use crate::compiler::{
    error::ResultWithDiagnostics,
    parser::node::{Literal, Node, VType},
    tokens::PrimitiveTypes,
};

pub fn analyze(
    root: &Node,
    interner: &StringInterner<BucketBackend<SymbolUsize>>,
    nodes: &mut Arena<Node>,
) -> ResultWithDiagnostics<HashMap<SymbolUsize, (Vec<VType>, VType)>> {
    let function_table = function_table::generate_function_table(nodes);
    let type_check_result = type_check::type_check_root(root, interner, nodes, &function_table);

    let mut result = ResultWithDiagnostics::new(function_table);
    if let Err(diagnostics) = type_check_result {
        result.extend_diagnostics(diagnostics);
    }

    result
}

pub fn literal_default_types(literal: Literal) -> VType {
    match literal {
        Literal::Int(_) => VType::Primitive(PrimitiveTypes::I32),
        Literal::Float(_) => VType::Primitive(PrimitiveTypes::F32),
        Literal::Bool(_) => VType::Primitive(PrimitiveTypes::Bool),
    }
}
