use cranelift::prelude::{EntityRef, FunctionBuilder, Value, Variable};
use generational_arena::Index;
use std::collections::HashMap;
use string_interner::symbol::SymbolUsize;

use crate::{
    SafeConvert,
    compiler::{
        codegen::{Info, error::TranslateError},
        parser::node::{ExprKind, NodeKind, Type},
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
    // TODO: allow binding to Nil/ other zero-sized value
    let var_val = var_val.expect("TODO: let binding requires non-zero-sized value. This will change later");
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
) -> Result<Value, TranslateError> {
    let var = scopes.iter().rev().find_map(|scope| scope.get(&sym));

    match var {
        Some((_, variable)) => Ok(fn_builder.use_var(*variable)),
        None => Err(TranslateError::UndeclaredIdentifier(format!("Identifier {:?}", sym))),
    }
}

pub fn expr_assign(
    target: Index,
    value: Index,
    fn_builder: &mut FunctionBuilder,
    scopes: &mut Vec<HashMap<SymbolUsize, (Type, Variable)>>,
    info: &mut Info,
) -> Result<Value, TranslateError> {
    let (maybe_var_val, actual_type) = expr_to_val(value, fn_builder, scopes, info)?;
    let var_val = maybe_var_val.expect("assignment requires non-zero-sized value");
    let ty = actual_type.to_clif(info.build_config.ptr_width);

    // Create a unique variable index across all scopes
    let var_index = scopes.iter().map(|s| s.len()).sum::<usize>();
    let var = Variable::new(var_index);
    fn_builder.declare_var(var, ty);
    fn_builder.def_var(var, var_val);
    let symbol = match &info.nodes.get(target).safe().kind {
        NodeKind::Expr { kind, .. } => match kind {
            ExprKind::Identifier(k) => k,
            _ => unreachable!(),
        },
        _ => unreachable!(),
    };

    scopes.last_mut().safe().insert(*symbol, (actual_type, var));

    Ok(var_val)
}
