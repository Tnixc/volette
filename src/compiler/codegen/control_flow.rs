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
) -> Result<Option<Value>, TranslateError> {
    if let Some(ret_val) = ret_val {
        let (value, _) = expr_to_val(ret_val, fn_builder, scopes, info)?;
        match value {
            Some(v) => fn_builder.ins().return_(&[v]),
            None => fn_builder.ins().return_(&[]),
        };
    } else {
        fn_builder.ins().return_(&[]);
    }
    Ok(None)
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

    let result_type = match info.nodes.get(then_idx).safe().kind {
        NodeKind::Expr { type_, .. } => type_,
        _ => unreachable!(),
    }
    .safe()
    .to_clif(info.build_config.ptr_width);

    let (cond_val, _) = expr_to_val(cond, fn_builder, scopes, info)?;
    let cond_val = cond_val.safe(); // should be caught by type checker

    fn_builder.ins().brif(cond_val, then_block, &[], else_block, &[]);

    // check if both branches terminate
    let then_terminates = expr_terminates(then_idx, info);
    let else_terminates = else_idx.map_or(false, |idx| expr_terminates(idx, info));
    let both_terminate = then_terminates && else_terminates;

    // then block
    fn_builder.switch_to_block(then_block);

    let (then_val, _) = expr_to_val(then_idx, fn_builder, scopes, info)?;
    fn_builder.seal_block(then_block);

    // else block
    fn_builder.switch_to_block(else_block);

    let else_val = if let Some(else_idx) = else_idx {
        let (val, _) = expr_to_val(else_idx, fn_builder, scopes, info)?;
        fn_builder.seal_block(else_block);
        val
    } else {
        fn_builder.seal_block(else_block);
        None
    };

    // if both branches terminate, switch to unreachable block
    if both_terminate {
        let dead_block = fn_builder.create_block();
        fn_builder.switch_to_block(dead_block);
        fn_builder.seal_block(dead_block);
        return Ok(Value::from_u32(0));
    }

    // create merge block since at least one branch continues
    let merge_block = fn_builder.create_block();
    fn_builder.append_block_param(merge_block, result_type);

    // add jumps from non-terminating branches
    if !then_terminates {
        fn_builder.switch_to_block(then_block);
        match then_val {
            Some(then_val) => fn_builder.ins().jump(merge_block, &[then_val]),
            None => {
                let default = fn_builder.ins().iconst(result_type, 0);
                fn_builder.ins().jump(merge_block, &[default])
            }
        };
    }

    if !else_terminates {
        fn_builder.switch_to_block(else_block);
        match else_val {
            Some(else_val) => fn_builder.ins().jump(merge_block, &[else_val]),
            None => {
                let default = fn_builder.ins().iconst(result_type, 0);
                fn_builder.ins().jump(merge_block, &[default])
            }
        };
    } else if else_idx.is_none() {
        // no else branch provided, jump with default value
        fn_builder.switch_to_block(else_block);
        let default_val = fn_builder.ins().iconst(result_type, 0);
        fn_builder.ins().jump(merge_block, &[default_val]);
    }

    fn_builder.switch_to_block(merge_block);
    fn_builder.seal_block(merge_block);

    let result = fn_builder.block_params(merge_block)[0];

    Ok(result)
}
