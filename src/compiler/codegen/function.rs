use crate::compiler::parser::node::{DefKind, Node, NodeKind};
use cranelift::{
    codegen::{
        ir::{Function, UserFuncName},
        Context,
    },
    module::{Linkage, Module},
    object::ObjectModule,
    prelude::{AbiParam, FunctionBuilder, FunctionBuilderContext, Signature},
};
use string_interner::{backend::BucketBackend, symbol::SymbolUsize, StringInterner};

use super::{error::CraneliftError, BuildConfig};

pub fn lower_fn(
    node: &Node,
    interner: StringInterner<BucketBackend<SymbolUsize>>,
    ctx: &mut Context,
    object: &mut ObjectModule,
    build_config: &BuildConfig,
) -> Result<(), CraneliftError> {
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
            let fn_name = interner
                .resolve(*name)
                .expect("Function name's string not found in interner");

            let mut sig = Signature::new(build_config.call_conv);
            for param in params {
                sig.params.push(AbiParam::new(param.1.to_clif(build_config.ptr_width)));
            }

            sig.returns
                .push(AbiParam::new(return_type.to_clif(build_config.ptr_width)));

            println!("Function: {}", fn_name);

            let func_id = object.declare_function(fn_name, Linkage::Export, &sig)?;

            let dbg_fn_name = UserFuncName::testcase(fn_name);
            let mut func = Function::with_name_signature(dbg_fn_name, sig);

            let mut fn_builder_ctx = FunctionBuilderContext::new();
            let mut fn_builder = FunctionBuilder::new(&mut func, &mut fn_builder_ctx);

            let entry = fn_builder.create_block();
            fn_builder.switch_to_block(entry);
            fn_builder.append_block_params_for_function_params(entry);

            Ok(())
        }
        _ => Ok(()),
    }
}
