use cranelift::module::Module;
use cranelift::prelude::{FunctionBuilder, InstBuilder, Value};
use generational_arena::Index;
use rootcause::prelude::*;

use crate::compiler::codegen::Scopes;
use crate::{
    SafeConvert,
    compiler::{
        codegen::Info,
        parser::node::{ExprKind, NodeKind, VType},
        tokens::PrimitiveTypes,
    },
};

use super::expr::expr_to_val;

pub fn expr_call(
    func: Index,
    args: &[Index],
    fn_builder: &mut FunctionBuilder,
    scopes: &mut Scopes,
    info: &mut Info,
) -> Result<Value, Report> {
    let func_node = info.nodes.get(func).safe();

    let func_name = match &func_node.kind {
        NodeKind::Expr {
            kind: ExprKind::Identifier(sym),
            ..
        } => *sym,
        _ => panic!("Function call target must be an identifier"),
    };

    let func_id = *info.func_table.get(&func_name).safe();

    let mut arg_values = Vec::new();
    for arg in args {
        let (arg_value, _) = expr_to_val(*arg, fn_builder, scopes, info)?;
        let arg_value = arg_value.expect("function arguments must be non-zero-sized");
        arg_values.push(arg_value);
    }

    let func_ref = info.module.declare_func_in_func(func_id, &mut fn_builder.func);

    let call_inst = fn_builder.ins().call(func_ref, &arg_values);

    // check if the function returns a zero-sized type (Nil or Never)
    let (_params, return_type) = info.fn_table.get(&func_name).safe();

    match return_type {
        // zero-sized return, return dummy value
        VType::Primitive(PrimitiveTypes::Nil) | VType::Primitive(PrimitiveTypes::Never) => Ok(Value::from_u32(0)),
        _ => {
            let result = fn_builder.inst_results(call_inst)[0];
            Ok(result)
        }
    }
}
