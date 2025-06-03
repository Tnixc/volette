use crate::compiler::{
    codegen::{block::lower_block, Info},
    parser::node::{DefKind, ExprKind, Node, NodeKind},
};
use cranelift::{
    codegen::{
        ir::{Function, UserFuncName},
        Context,
    },
    module::{Linkage, Module},
    object::ObjectModule,
    prelude::{AbiParam, FunctionBuilder, FunctionBuilderContext, Signature, Variable},
};
use hashbrown::HashMap;
use string_interner::symbol::SymbolUsize;

use super::{error::TranslateError, BuildConfig};

pub fn lower_fn(node: &Node, info: &mut Info) -> Result<(), TranslateError> {
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
            let fn_name = info
                .interner
                .resolve(*name)
                .expect("Function name's string not found in interner");

            let mut sig = Signature::new(info.build_config.call_conv);
            for param in params {
                sig.params
                    .push(AbiParam::new(param.1.to_clif(info.build_config.ptr_width)));
            }

            sig.returns
                .push(AbiParam::new(return_type.to_clif(info.build_config.ptr_width)));

            println!("Function: {}", fn_name);

            let func_id = info.module.declare_function(fn_name, Linkage::Export, &sig)?;

            let dbg_fn_name = UserFuncName::testcase(fn_name);
            let mut func = Function::with_name_signature(dbg_fn_name, sig);

            let mut fn_builder_ctx = FunctionBuilderContext::new();
            let mut fn_builder = FunctionBuilder::new(&mut func, &mut fn_builder_ctx);

            let entry = fn_builder.create_block();

            fn_builder.switch_to_block(entry);
            fn_builder.append_block_params_for_function_params(entry);

            // -- Body --

            let body_node = info.nodes.get(*body).expect("Body node not found in arena");

            let mut scopes: Vec<HashMap<SymbolUsize, Variable>> = vec![]; // fresh scope for each function
            lower_block(body_node, &mut fn_builder, info, &mut scopes)?;

            // fn_builder.seal_block(entry);
            // fn_builder.finalize();

            // info.ctx.func = func;
            // println!("{}", info.ctx.func.display());

            Ok(())
        }
        _ => Ok(()),
    }
}
