use std::{collections::HashMap, sync::Arc};

use cranelift::{
    codegen::Context,
    module::{Linkage, Module, default_libcall_names},
    object::{self, ObjectModule},
    prelude::{
        AbiParam, Signature,
        isa::{self, CallConv, TargetIsa},
        settings::{self, Flags},
    },
};
use generational_arena::Arena;
use string_interner::{StringInterner, backend::BucketBackend, symbol::SymbolUsize};
use target_lexicon::{PointerWidth as TargetPointerWidth, Triple};

use crate::{
    SafeConvert,
    compiler::{
        codegen::{error::TranslateError, function::lower_fn},
        parser::node::{DefKind, Node, NodeKind, Type},
    },
};

pub mod binary_ops;
pub mod block;
pub mod control_flow;
pub mod error;
pub mod expr;
pub mod function;
pub mod function_call;
pub mod literal;
pub mod translate;
pub mod variable;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum PtrWidth {
    X32,
    X64,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct BuildConfig {
    pub ptr_width: PtrWidth,
    pub call_conv: CallConv,
}

pub struct Info<'a> {
    pub module: ObjectModule,
    pub ctx: Context,
    pub build_config: BuildConfig,
    pub nodes: &'a Arena<Node>,
    pub interner: &'a StringInterner<BucketBackend<SymbolUsize>>,
    pub func_table: HashMap<SymbolUsize, cranelift::module::FuncId>,
}

pub fn codegen(
    root: &Node,
    nodes: &Arena<Node>,
    interner: &StringInterner<BucketBackend<SymbolUsize>>,
    _fn_table: &HashMap<SymbolUsize, (Box<Vec<Type>>, Type)>,
) -> Result<(), TranslateError> {
    let (target_isa, call_conv, ptr_width) = isa();
    let builder = object::ObjectBuilder::new(target_isa, "vtlib", default_libcall_names())?;

    let module = ObjectModule::new(builder);
    let ctx = Context::new();

    let build_config = BuildConfig { ptr_width, call_conv };

    let mut info = Info {
        module,
        ctx,
        build_config,
        nodes,
        interner,
        func_table: HashMap::new(),
    };

    match &root.kind {
        NodeKind::Root { defs } => {
            // declare all functions first
            let mut func_ids = Vec::new();
            for def in defs {
                let def_node = nodes.get(*def).safe();
                match &def_node.kind {
                    NodeKind::Def { kind } => match kind {
                        DefKind::Function {
                            name, params, return_type, ..
                        } => {
                            let fn_name = info.interner.resolve(*name).safe();

                            let mut sig = Signature::new(info.build_config.call_conv);
                            for param in params {
                                sig.params.push(AbiParam::new(param.1.to_clif(info.build_config.ptr_width)));
                            }
                            sig.returns.push(AbiParam::new(return_type.to_clif(info.build_config.ptr_width)));

                            let func_id = info.module.declare_function(fn_name, Linkage::Export, &sig)?;
                            func_ids.push(func_id);
                            info.func_table.insert(*name, func_id);
                        }
                        _ => {
                            return Err(TranslateError::Internal(
                                "Only function definitions are currently supported".to_string(),
                            ));
                        }
                    },
                    _ => {
                        return Err(TranslateError::Internal(
                            "Only definition nodes are currently supported".to_string(),
                        ));
                    }
                }
            }

            // define function bodies
            for (i, def) in defs.iter().enumerate() {
                let def_node = nodes.get(*def).safe();
                match &def_node.kind {
                    NodeKind::Def { kind } => match kind {
                        DefKind::Function { .. } => {
                            let func_id = func_ids[i];
                            lower_fn(def_node, &mut info, func_id)?;
                            info.module.define_function(func_id, &mut info.ctx)?;
                        }
                        _ => {
                            return Err(TranslateError::Internal(
                                "Only function definitions are currently supported".to_string(),
                            ));
                        }
                    },
                    _ => {
                        return Err(TranslateError::Internal(
                            "Only definition nodes are currently supported".to_string(),
                        ));
                    }
                }
            }
        }
        _ => return Err(TranslateError::Internal("Expected root node in codegen".to_string())),
    }

    let product = info.module.finish();
    let obj_bytes = product.emit()?;

    std::fs::write("vtlib.o", obj_bytes).expect("Couldn't write");
    println!("Object file 'vtlib.o' emitted.");
    Ok(())
}

fn isa() -> (Arc<dyn TargetIsa + 'static>, CallConv, PtrWidth) {
    let triple = Triple::host();

    let flag_builder = settings::builder();
    let flags = Flags::new(flag_builder);

    match isa::lookup(triple.clone()) {
        Ok(isa_builder) => match isa_builder.finish(flags) {
            Ok(isa) => {
                let call_conv = isa.default_call_conv();

                let ptr_width = match triple.pointer_width() {
                    Ok(TargetPointerWidth::U16) => PtrWidth::X32,
                    Ok(TargetPointerWidth::U32) => PtrWidth::X32,
                    Ok(TargetPointerWidth::U64) => PtrWidth::X64,
                    Err(_) => PtrWidth::X64,
                };

                (isa, call_conv, ptr_width)
            }
            Err(e) => panic!("failed to create ISA for {}: {}", triple, e),
        },
        Err(e) => panic!("target {} is not supported: {}", triple, e),
    }
}
