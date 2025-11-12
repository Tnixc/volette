mod binop;
pub mod error;
mod function_table;
mod type_check;
mod unaryop;

use std::collections::HashMap;

use generational_arena::Arena;
use string_interner::{StringInterner, backend::BucketBackend, symbol::SymbolUsize};

use crate::compiler::{
    error::ResultWithDiagnostics,
    parser::node::{Literal, Node, Type},
    tokens::PrimitiveTypes,
};

pub fn analysis_pass(
    root: &Node,
    interner: &StringInterner<BucketBackend<SymbolUsize>>,
    mut nodes: &mut Arena<Node>,
) -> ResultWithDiagnostics<HashMap<SymbolUsize, (Box<Vec<Type>>, Type)>> {
    let function_table = function_table::generate_function_table(&mut nodes);
    let type_check_result = type_check::type_check_root(root, &interner, &mut nodes, &function_table);

    let mut result = ResultWithDiagnostics::new(function_table);
    if let Err(diagnostics) = type_check_result {
        result.extend_diagnostics(diagnostics);
    }

    result
}

pub fn literal_default_types(literal: Literal) -> Type {
    match literal {
        Literal::Int(_) => Type::Primitive(PrimitiveTypes::I32),
        Literal::Float(_) => Type::Primitive(PrimitiveTypes::F32),
        Literal::Bool(_) => Type::Primitive(PrimitiveTypes::Bool),
        Literal::Nil => Type::Primitive(PrimitiveTypes::Nil),
    }
}
