use cranelift::prelude::{FunctionBuilder, InstBuilder, Value, Variable};
use generational_arena::Index;
use std::collections::HashMap;
use string_interner::symbol::SymbolUsize;

use crate::compiler::{
    codegen::{Info, error::TranslateError},
    parser::node::Type,
};

use super::expr::expr_to_val;

pub fn expr_return(
    ret_val: Option<Index>,
    fn_builder: &mut FunctionBuilder,
    scopes: &mut Vec<HashMap<SymbolUsize, (Type, Variable)>>,
    info: &mut Info,
) -> Result<Value, TranslateError> {
    if let Some(ret_val) = ret_val {
        let (value, _) = expr_to_val(ret_val, fn_builder, scopes, info)?;
        fn_builder.ins().return_(&[value]);
        Ok(value)
    } else {
        fn_builder.ins().return_(&[]);
        Ok(Value::from_u32(0)) // TODO: implement proper never type
    }
}
