use cranelift::prelude::{FunctionBuilder, InstBuilder, Value, types};
use generational_arena::Index;
use std::collections::HashMap;

use crate::compiler::{
    codegen::{Info, Scopes, error::TranslateError},
    parser::node::Type,
    tokens::PrimitiveTypes,
};

use super::expr::expr_to_val;

pub fn expr_block(
    exprs: &[Index],
    fn_builder: &mut FunctionBuilder,
    scopes: &mut Scopes,
    info: &mut Info,
) -> Result<Value, TranslateError> {
    scopes.push(HashMap::new());
    let mut last_res: Option<(Option<Value>, Type)> = None;
    for expr in exprs {
        last_res = Some(expr_to_val(*expr, fn_builder, scopes, info)?);
    }
    scopes.pop();

    match last_res {
        Some((Some(val), _)) => Ok(val),
        Some((None, Type::Primitive(PrimitiveTypes::Never))) => {
            // TODO: check if this makes sense
            // block terminated with never type (e.g., return), return dummy value
            Ok(Value::from_u32(0))
        }
        Some((None, _)) | None => {
            // zero-sized value or empty block, emit dummy constant
            Ok(fn_builder.ins().iconst(types::I32, 0))
        }
    }
}
