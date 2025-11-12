use cranelift::prelude::{FunctionBuilder, StackSlotData, StackSlotKind, Value};
use generational_arena::Index;
use string_interner::symbol::SymbolUsize;

use crate::{
    SafeConvert,
    compiler::{
        codegen::{Info, Scopes, error::TranslateError},
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
) -> Result<Value, TranslateError> {
    let (var_val, actual_type) = expr_to_val(value, fn_builder, scopes, info)?;
    // TODO: allow binding to Nil/ other zero-sized value
    let var_val = var_val.expect("TODO: let binding requires non-zero-sized value. This will change later");
    let ty = actual_type.to_clif(info.build_config.ptr_width);

    let stack_slot = fn_builder.create_sized_stack_slot(StackSlotData::new(StackSlotKind::ExplicitSlot, ty.bytes(), 0));
    // WARNING: idk if it's right to always set offset as 0

    // Create a unique variable index across all scopes
    // let var_index = scopes.iter().map(|s| s.len()).sum::<usize>();
    // let var = Variable::new(var_index);
    let var = fn_builder.declare_var(ty);
    fn_builder.def_var(var, var_val);

    scopes.last_mut().safe().insert(name, (actual_type, var, stack_slot));

    Ok(var_val)
}

pub fn expr_identifier(
    sym: SymbolUsize,
    fn_builder: &mut FunctionBuilder,
    scopes: &mut Scopes,
    info: &Info,
) -> Result<Value, TranslateError> {
    let var = scopes.iter().rev().find_map(|scope| scope.get(&sym));

    match var {
        Some((_, variable, _)) => Ok(fn_builder.use_var(*variable)),
        None => {
            use crate::compiler::tokens::DisplaySpan;
            Err(TranslateError::NotFound {
                what: format!("identifier '{}'", info.interner.resolve(sym).unwrap_or("<unknown>")),
                span: DisplaySpan {
                    file: "<internal>".to_string(),
                    start: (0, 0),
                    end: (0, 0),
                },
            })
        }
    }
}

pub fn expr_assign(
    target: Index,
    value: Index,
    fn_builder: &mut FunctionBuilder,
    scopes: &mut Scopes,
    info: &mut Info,
) -> Result<Value, TranslateError> {
    let (maybe_var_val, actual_type) = expr_to_val(value, fn_builder, scopes, info)?;
    let var_val = maybe_var_val.expect("assignment requires non-zero-sized value");
    let ty = actual_type.to_clif(info.build_config.ptr_width);

    // FIXME: mutation should not reassign?
    let stack_slot = fn_builder.create_sized_stack_slot(StackSlotData::new(StackSlotKind::ExplicitSlot, ty.bytes(), 0));

    // Create a unique variable index across all scopes
    // let var_index = scopes.iter().map(|s| s.len()).sum::<usize>();
    // let var = Variable::new(var_index);
    let var = fn_builder.declare_var(ty);
    fn_builder.def_var(var, var_val);
    let symbol = match &info.nodes.get(target).safe().kind {
        NodeKind::Expr { kind, .. } => match kind {
            ExprKind::Identifier(k) => k,
            _ => unreachable!(),
        },
        _ => unreachable!(),
    };

    scopes.last_mut().safe().insert(*symbol, (actual_type, var, stack_slot));

    Ok(var_val)
}
