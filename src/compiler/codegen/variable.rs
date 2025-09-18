use cranelift::prelude::{EntityRef, FunctionBuilder, Value, Variable};
use generational_arena::Index;
use std::collections::HashMap;
use string_interner::symbol::SymbolUsize;

use crate::{
    SafeConvert,
    compiler::{
        codegen::{Info, error::TranslateError},
        parser::node::Type,
    },
};

use super::expr::expr_to_val;

pub fn expr_let_binding(
    name: SymbolUsize,
    value: Index,
    fn_builder: &mut FunctionBuilder,
    scopes: &mut Vec<HashMap<SymbolUsize, (Type, Variable)>>,
    info: &mut Info,
) -> Result<Value, TranslateError> {
    let (var_val, actual_type) = expr_to_val(value, fn_builder, scopes, info)?;
    let ty = actual_type.to_clif(info.build_config.ptr_width);

    // Create a unique variable index across all scopes
    let var_index = scopes.iter().map(|s| s.len()).sum::<usize>();
    let var = Variable::new(var_index);
    fn_builder.declare_var(var, ty);
    fn_builder.def_var(var, var_val);

    scopes.last_mut().safe().insert(name, (actual_type, var));

    Ok(var_val)
}

pub fn expr_identifier(
    sym: SymbolUsize,
    fn_builder: &mut FunctionBuilder,
    scopes: &mut Vec<HashMap<SymbolUsize, (Type, Variable)>>,
) -> Value {
    let var = scopes.iter().rev().find_map(|scope| scope.get(&sym)).safe();
    println!("Variable: {:?}", var);
    fn_builder.use_var(var.1)
}
