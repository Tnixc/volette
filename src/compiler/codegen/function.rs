use crate::{
    SafeConvert,
    compiler::{
        codegen::{Info, expr::expr_to_val},
        parser::node::{DefKind, Node, NodeKind, Type},
        tokens::PrimitiveTypes,
    },
};
use cranelift::{
    codegen::ir::{Function, UserFuncName},
    module::FuncId,
    prelude::{AbiParam, FunctionBuilder, FunctionBuilderContext, InstBuilder, Signature, Variable},
};
use std::collections::HashMap;
use string_interner::symbol::SymbolUsize;

use super::error::TranslateError;

pub fn lower_fn(node: &Node, info: &mut Info, _func_id: FuncId) -> Result<(), TranslateError> {
    match &node.kind {
        NodeKind::Def {
            kind:
                DefKind::Function {
                    name,
                    params,
                    body,
                    return_type,
                },
        } => {
            // -- Setup --
            let fn_name = info.interner.resolve(*name).safe();

            // make the signature again
            let mut sig = Signature::new(info.build_config.call_conv);
            for param in params {
                sig.params.push(AbiParam::new(param.1.to_clif(info.build_config.ptr_width)));
            }
            // only add return type if it's not zero-sized (Nil or Never)
            match return_type {
                Type::Primitive(PrimitiveTypes::Nil) | Type::Primitive(PrimitiveTypes::Never) => {
                    // TODO: check if this is correct
                    // zero-sized types don't need a return value in the signature
                }
                _ => {
                    sig.returns.push(AbiParam::new(return_type.to_clif(info.build_config.ptr_width)));
                }
            }

            let dbg_fn_name = UserFuncName::testcase(fn_name);
            let mut func = Function::with_name_signature(dbg_fn_name, sig);

            let mut fn_builder_ctx = FunctionBuilderContext::new();
            let mut fn_builder = FunctionBuilder::new(&mut func, &mut fn_builder_ctx);

            let entry = fn_builder.create_block();

            fn_builder.switch_to_block(entry);
            fn_builder.append_block_params_for_function_params(entry);

            // -- Body --
            let mut scopes: Vec<HashMap<SymbolUsize, (Type, Variable)>> = vec![HashMap::new()];

            // collect block parameters into a vector to avoid borrowing conflicts
            let block_params: Vec<_> = fn_builder.block_params(entry).to_vec();

            for (i, param) in params.iter().enumerate() {
                let val = block_params[i];
                // let var = Variable::new(i);
                let ty = param.1.to_clif(info.build_config.ptr_width);

                let var = fn_builder.declare_var(ty);
                fn_builder.def_var(var, val);
                scopes.last_mut().safe().insert(param.0, (param.1.clone(), var));
            }

            let (maybe_body_val, body_type) = expr_to_val(*body, &mut fn_builder, &mut scopes, info)?;

            // add implicit return if the body doesn't diverge (never type)
            if body_type != Type::Primitive(PrimitiveTypes::Never) {
                match maybe_body_val {
                    Some(body_val) => fn_builder.ins().return_(&[body_val]),
                    None => fn_builder.ins().return_(&[]),
                };
            }

            fn_builder.seal_block(entry);
            fn_builder.finalize();

            info.ctx.func = func;
            println!("{}", info.ctx.func.display());

            Ok(())
        }
        _ => {
            return Err(TranslateError::Unsupported {
                what: "non-function definition".to_string(),
                reason: "only function definitions are currently supported".to_string(),
                span: node.span.to_display(info.interner),
            });
        }
    }
}
