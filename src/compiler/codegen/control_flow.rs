use cranelift::prelude::{FunctionBuilder, InstBuilder, Value, Variable};
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

/// checks if an expression terminates the current block (e.g., contains a return)
/// i.e. you cannot add any more instructions
fn expr_terminates(node_idx: Index, info: &Info) -> bool {
    let node = info.nodes.get(node_idx).safe();
    match &node.kind {
        NodeKind::Expr { kind, .. } => match kind {
            ExprKind::Return { .. } => true,
            ExprKind::Block { exprs } => {
                // a block terminates if its last expression terminates... maybe? TODO: check this
                exprs.last().map_or(false, |last| expr_terminates(*last, info))
            }
            ExprKind::If {
                then_block, else_block, ..
            } => {
                // an if terminates only if both branches terminate
                let then_term = expr_terminates(*then_block, info);
                let else_term = else_block.map_or(false, |e| expr_terminates(e, info));
                then_term && else_term
            }
            _ => false,
        },
        _ => false,
    }
}

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

pub fn expr_if(
    cond: Index,
    then_idx: Index,
    else_idx: Option<Index>,
    fn_builder: &mut FunctionBuilder,
    scopes: &mut Vec<HashMap<SymbolUsize, (Type, Variable)>>,
    info: &mut Info,
) -> Result<Value, TranslateError> {
    let then_block = fn_builder.create_block();
    let else_block = fn_builder.create_block();
    let merge_block = fn_builder.create_block();

    let result_type = match info.nodes.get(then_idx).safe().kind {
        NodeKind::Expr { type_, .. } => type_,
        _ => unreachable!(),
    }
    .safe()
    .to_clif(info.build_config.ptr_width);
    fn_builder.append_block_param(merge_block, result_type);

    let (cond_val, _) = expr_to_val(cond, fn_builder, scopes, info)?;

    // (done in caller block)
    fn_builder.ins().brif(cond_val, then_block, &[], else_block, &[]);

    // then block
    fn_builder.switch_to_block(then_block);
    fn_builder.seal_block(then_block);

    let then_terminates = expr_terminates(then_idx, info);
    let (then_val, _) = expr_to_val(then_idx, fn_builder, scopes, info)?;
    if !then_terminates {
        fn_builder.ins().jump(merge_block, &[then_val]);
    }

    fn_builder.switch_to_block(else_block);
    fn_builder.seal_block(else_block);
    if let Some(else_idx) = else_idx {
        let else_terminates = expr_terminates(else_idx, info);
        let (else_val, _) = expr_to_val(else_idx, fn_builder, scopes, info)?;
        if !else_terminates {
            fn_builder.ins().jump(merge_block, &[else_val]);
        }
    } else {
        // TODO!!!! needs else block
        let default_val = fn_builder.ins().iconst(result_type, 0);
        fn_builder.ins().jump(merge_block, &[default_val]);
    }

    fn_builder.switch_to_block(merge_block);
    fn_builder.seal_block(merge_block);

    let result = fn_builder.block_params(merge_block)[0];

    Ok(result)
}
