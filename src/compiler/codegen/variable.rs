use cranelift::prelude::{FunctionBuilder, InstBuilder, StackSlotData, StackSlotKind, Value};
use generational_arena::Index;
use rootcause::prelude::*;
use string_interner::symbol::SymbolUsize;

use crate::{
    SafeConvert,
    compiler::{
        codegen::{Info, Scopes, ptr_width},
        error::Help,
        parser::node::{ExprKind, NodeKind},
    },
};

use super::expr::expr_to_val;

pub fn expr_let_binding(
    name: SymbolUsize,
    value: Index,
    fn_builder: &mut FunctionBuilder,
    scopes: &mut Scopes,
    info: &mut Info,
) -> Result<Value, Report> {
    let (var_val, actual_type) = expr_to_val(value, fn_builder, scopes, info)?;
    let var_val = var_val.expect("TODO: let binding requires non-zero-sized value");
    let ty = actual_type.to_clif(ptr_width());

    let size = ty.bytes();
    let slot = fn_builder.create_sized_stack_slot(StackSlotData::new(StackSlotKind::ExplicitSlot, size, 0));
    fn_builder.ins().stack_store(var_val, slot, 0);

    scopes.last_mut().safe().insert(name, (actual_type, slot));

    Ok(var_val)
}

pub fn expr_identifier(sym: SymbolUsize, fn_builder: &mut FunctionBuilder, scopes: &mut Scopes, info: &Info) -> Result<Value, Report> {
    let var = scopes.iter().rev().find_map(|scope| scope.get(&sym));

    match var {
        Some((vtype, slot)) => {
            let ty = vtype.to_clif(ptr_width());
            Ok(fn_builder.ins().stack_load(ty, *slot, 0))
        }
        None => Err(crate::codegen_err!(
            "Unresolved identifier '{}'",
            None,
            info.interner.resolve(sym).unwrap_or("<unknown>")
        )
        .attach(Help("This identifier or symbol was not found in the current scope".into()))),
    }
}

pub fn expr_assign(
    target: Index,
    value: Index,
    fn_builder: &mut FunctionBuilder,
    scopes: &mut Scopes,
    info: &mut Info,
) -> Result<Value, Report> {
    let (maybe_var_val, _) = expr_to_val(value, fn_builder, scopes, info)?;
    let var_val = maybe_var_val.expect("assignment requires non-zero-sized value");

    let symbol = match &info.nodes.get(target).safe().kind {
        NodeKind::Expr { kind, .. } => match kind {
            ExprKind::Identifier(k) => k,
            _ => unreachable!(),
        },
        _ => unreachable!(),
    };

    let slot = scopes
        .iter()
        .rev()
        .find_map(|scope| scope.get(symbol))
        .map(|(_, slot)| *slot)
        .expect("assignment to undeclared variable");

    fn_builder.ins().stack_store(var_val, slot, 0);

    Ok(var_val)
}
