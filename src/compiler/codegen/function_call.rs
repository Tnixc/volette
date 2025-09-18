use cranelift::module::Module;
use cranelift::prelude::{FunctionBuilder, InstBuilder, Value, Variable};
use generational_arena::Index;
use std::collections::HashMap;
use string_interner::symbol::SymbolUsize;

use crate::compiler::{
    codegen::{Info, error::TranslateError},
    parser::node::{ExprKind, NodeKind, Type},
};

use super::expr::expr_to_val;

pub fn expr_call(
    func: Index,
    args: &[Index],
    fn_builder: &mut FunctionBuilder,
    scopes: &mut Vec<HashMap<SymbolUsize, (Type, Variable)>>,
    info: &mut Info,
) -> Result<Value, TranslateError> {
    let func_node = info.nodes.get(func).expect("[Function call] Function node not found in arena");

    let func_name = match &func_node.kind {
        NodeKind::Expr {
            kind: ExprKind::Identifier(sym),
            ..
        } => *sym,
        _ => panic!("Function call target must be an identifier"),
    };

    let func_id = *info.func_table.get(&func_name).expect("Function not found in function table");

    let mut arg_values = Vec::new();
    for arg in args {
        let (arg_value, _arg_type) = expr_to_val(*arg, fn_builder, scopes, info)?;
        arg_values.push(arg_value);
    }

    let func_ref = info.module.declare_func_in_func(func_id, &mut fn_builder.func);

    let call_inst = fn_builder.ins().call(func_ref, &arg_values);

    let result = fn_builder.inst_results(call_inst)[0];

    Ok(result)
}
