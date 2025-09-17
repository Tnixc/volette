mod error;
mod function_table;
mod type_check;

use generational_arena::Arena;
use string_interner::{StringInterner, backend::BucketBackend, symbol::SymbolUsize};

use crate::compiler::{
    parser::node::{Literal, Node, Type},
    tokens::PrimitiveTypes,
};

pub fn analysis_pass(root: &Node, interner: &StringInterner<BucketBackend<SymbolUsize>>, mut nodes: &mut Arena<Node>) {
    let function_table = function_table::generate_function_table(&mut nodes);
    type_check::type_check_root(root, &interner, &mut nodes, function_table);
}

pub fn literal_default_types(literal: Literal) -> Type {
    match literal {
        Literal::Int(_) => Type::Primitive(PrimitiveTypes::I32),
        Literal::Float(_) => Type::Primitive(PrimitiveTypes::F32),
        Literal::Bool(_) => Type::Primitive(PrimitiveTypes::Bool),
        Literal::Nil => Type::Primitive(PrimitiveTypes::Nil),
    }
}
