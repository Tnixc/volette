use cranelift::{
    codegen::ir::BlockArg,
    prelude::{FunctionBuilder, InstBuilder, Value},
};
use generational_arena::Index;
use rootcause::prelude::*;

use crate::{
    SafeConvert,
    compiler::{
        codegen::{Info, Scopes, ptr_width},
        parser::node::{ExprKind, NodeKind},
    },
};

use super::expr::expr_to_val;

/// checks if an expression terminates the current block (e.g, contains a return)
/// that is, you cannot add any more instructions
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
    scopes: &mut Scopes,
    info: &mut Info,
) -> Result<Option<Value>, Report> {
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
    if_node_idx: Index,
    cond: Index,
    then_idx: Index,
    else_idx: Option<Index>,
    fn_builder: &mut FunctionBuilder,
    scopes: &mut Scopes,
    info: &mut Info,
) -> Result<Value, Report> {
    let then_block = fn_builder.create_block();
    let else_block = fn_builder.create_block();

    // get the result type from the if expression itself
    let result_type = match &info.nodes.get(if_node_idx).safe().kind {
        NodeKind::Expr { type_, .. } => type_.clone(),
        _ => unreachable!(),
    }
    .safe()
    .to_clif(ptr_width());

    let (cond_val, _) = expr_to_val(cond, fn_builder, scopes, info)?;
    let cond_val = cond_val.safe(); // should be caught by type checker

    fn_builder.ins().brif(cond_val, then_block, &[], else_block, &[]);

    // check if both branches terminate
    let then_terminates = expr_terminates(then_idx, info);
    let else_terminates = else_idx.map_or(false, |idx| expr_terminates(idx, info));
    let both_terminate = then_terminates && else_terminates;

    // check if result type is valid (not nil or never)
    use cranelift::prelude::types;
    let has_value = result_type != types::INVALID;

    // create merge block if at least one branch continues
    let merge_block = if !both_terminate {
        let mb = fn_builder.create_block();
        // Only add block parameter if the type produces a value
        if has_value {
            fn_builder.append_block_param(mb, result_type);
        }
        Some(mb)
    } else {
        None
    };

    // then block
    fn_builder.switch_to_block(then_block);
    let (then_val, _) = expr_to_val(then_idx, fn_builder, scopes, info)?;

    // seal the block if it terminates (has a return)
    if then_terminates {
        fn_builder.seal_block(then_block);
    }

    // add jump if this branch doesn't terminate
    if !then_terminates {
        let merge = merge_block.unwrap();
        if has_value {
            let val = then_val.unwrap_or_else(|| fn_builder.ins().iconst(result_type, 0));
            fn_builder.ins().jump(merge, &[BlockArg::Value(val)]);
        } else {
            // No value to pass for Nil/Never types
            fn_builder.ins().jump(merge, &[]);
        }
        fn_builder.seal_block(then_block);
    }

    // else block
    fn_builder.switch_to_block(else_block);
    let else_val = if let Some(else_idx) = else_idx {
        let (val, _) = expr_to_val(else_idx, fn_builder, scopes, info)?;
        val
    } else {
        None
    };

    // seal the block if it terminates (has a return)
    if else_terminates {
        fn_builder.seal_block(else_block);
    }

    // add jump if this branch doesn't terminate
    if !else_terminates {
        let merge = merge_block.unwrap();
        if has_value {
            let val = else_val.unwrap_or_else(|| fn_builder.ins().iconst(result_type, 0));
            fn_builder.ins().jump(merge, &[BlockArg::Value(val)]);
        } else {
            // No value to pass for Nil/Never types
            fn_builder.ins().jump(merge, &[]);
        }
        fn_builder.seal_block(else_block);
    } else if else_idx.is_none() {
        // no else branch provided, jump with default value (or no value for Nil)
        let merge = merge_block.unwrap();
        if has_value {
            let default_val = fn_builder.ins().iconst(result_type, 0);
            fn_builder.ins().jump(merge, &[BlockArg::Value(default_val)]);
        } else {
            fn_builder.ins().jump(merge, &[]);
        }
        fn_builder.seal_block(else_block);
    }

    // if both branches terminate, we need to switch to a new block but the value doesn't matter
    // since this code is unreachable

    if both_terminate {
        // return a dummy value that won't actually be used (the type is never)
        return Ok(Value::from_u32(0));
    }

    let merge_block = merge_block.unwrap();
    fn_builder.switch_to_block(merge_block);
    fn_builder.seal_block(merge_block);

    if has_value {
        let result = fn_builder.block_params(merge_block)[0];
        Ok(result)
    } else {
        // dummy value
        Ok(Value::from_u32(0)) // FIXME: idkkkkk
    }
}
