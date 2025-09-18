use cranelift::prelude::{FunctionBuilder, InstBuilder, Value, Variable, types};
use generational_arena::Index;
use std::collections::HashMap;
use string_interner::symbol::SymbolUsize;

use crate::compiler::{
    codegen::{Info, error::TranslateError},
    parser::node::Type,
    tokens::PrimitiveTypes,
};

use super::expr::expr_to_val;

pub fn expr_block(
    exprs: &[Index],
    fn_builder: &mut FunctionBuilder,
    scopes: &mut Vec<HashMap<SymbolUsize, (Type, Variable)>>,
    info: &mut Info,
) -> Result<(Value, Type), TranslateError> {
    scopes.push(HashMap::new());
    let mut last_res: Option<(Value, Type)> = None;
    for expr in exprs {
        last_res = Some(expr_to_val(*expr, fn_builder, scopes, info)?);
    }
    scopes.pop();

    Ok(last_res.unwrap_or_else(|| (fn_builder.ins().iconst(types::I32, 0), Type::Primitive(PrimitiveTypes::Nil))))
}
