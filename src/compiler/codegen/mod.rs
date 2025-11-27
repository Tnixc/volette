use std::{
    collections::HashMap,
    sync::{Arc, OnceLock},
};

use cranelift::{
    codegen::{Context, control::ControlPlane, ir::StackSlot},
    module::{Linkage, Module, default_libcall_names},
    object::{self, ObjectModule},
    prelude::{
        AbiParam, Signature,
        isa::{self, CallConv, TargetIsa},
        settings::{self, Flags},
        types::{I32, I64},
    },
};
use generational_arena::Arena;
use string_interner::{StringInterner, backend::BucketBackend, symbol::SymbolUsize};
use target_lexicon::{PointerWidth as TargetPointerWidth, Triple};

use crate::{
    SafeConvert,
    compiler::{
        codegen::function::lower_fn,
        error::{Help, ReportCollection},
        parser::node::{DefKind, Node, NodeKind, VType},
    },
};
use rootcause::prelude::*;

mod binary_ops;
mod block;
mod cast;
mod control_flow;
mod expr;
mod function;
mod function_call;
mod literal;
mod translate;
mod unary_ops;
mod variable;
mod r#while;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum PtrWidth {
    X32,
    X64,
}

impl PtrWidth {
    fn to_clif(self) -> cranelift::prelude::Type {
        match self {
            PtrWidth::X32 => I32,
            PtrWidth::X64 => I64,
        }
    }
}

static PTR_WIDTH: OnceLock<PtrWidth> = OnceLock::new();

pub fn ptr_width() -> PtrWidth {
    *PTR_WIDTH.get_or_init(|| {
        let triple = Triple::host();
        match triple.pointer_width() {
            Ok(TargetPointerWidth::U16) => PtrWidth::X32,
            Ok(TargetPointerWidth::U32) => PtrWidth::X32,
            Ok(TargetPointerWidth::U64) => PtrWidth::X64,
            Err(_) => PtrWidth::X64,
        }
    })
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct BuildConfig {
    pub call_conv: CallConv,
}

pub type Scopes = Vec<HashMap<SymbolUsize, (VType, StackSlot)>>;

pub struct Info<'a> {
    pub module: ObjectModule,
    pub ctx: Context,
    pub build_config: BuildConfig,
    pub nodes: &'a Arena<Node>,
    pub interner: &'a StringInterner<BucketBackend<SymbolUsize>>,
    pub func_table: HashMap<SymbolUsize, cranelift::module::FuncId>,
    pub fn_table: &'a HashMap<SymbolUsize, (Box<Vec<VType>>, VType)>,
    pub diagnostics: ReportCollection,
}

pub fn codegen(
    root: &Node,
    nodes: &Arena<Node>,
    interner: &StringInterner<BucketBackend<SymbolUsize>>,
    fn_table: &HashMap<SymbolUsize, (Box<Vec<VType>>, VType)>,
) -> Result<ReportCollection, Report> {
    let (target_isa, call_conv) = isa();
    let builder = object::ObjectBuilder::new(target_isa, "vtlib", default_libcall_names())
        .map_err(|e| crate::codegen_err!("Object builder error: {:?}", None, e))?;

    let module = ObjectModule::new(builder);
    let ctx = Context::new();

    let build_config = BuildConfig { call_conv };

    let mut info = Info {
        module,
        ctx,
        build_config,
        nodes,
        interner,
        func_table: HashMap::new(),
        fn_table,
        diagnostics: ReportCollection::new(),
    };

    info.ctx.want_disasm = true;
    info.ctx.set_disasm(true);

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
                                sig.params.push(AbiParam::new(param.1.to_clif(ptr_width())));
                            }
                            sig.returns.push(AbiParam::new(return_type.to_clif(ptr_width())));

                            let func_id = info.module.declare_function(fn_name, Linkage::Export, &sig).map_err(|e| {
                                crate::codegen_err!("Cranelift module error: {:?}", Some(def_node.span.to_display(interner)), e)
                            })?;
                            func_ids.push(func_id);
                            info.func_table.insert(*name, func_id);
                        }
                        _ => {
                            return Err(crate::codegen_err!(
                                "Unsupported non-function definition: only function definitions are currently supported",
                                Some(def_node.span.to_display(interner))
                            )
                            .attach(Help("This feature is not yet implemented".into())));
                        }
                    },
                    _ => {
                        return Err(crate::codegen_err!(
                            "Invalid node: only definition nodes are currently supported",
                            Some(def_node.span.to_display(interner))
                        )
                        .attach(Help("This construct is not valid in the current context".into())));
                    }
                }
            }

            // define function bodies
            let mut had_error = false;
            for (i, def) in defs.iter().enumerate() {
                let def_node = nodes.get(*def).safe();
                match &def_node.kind {
                    NodeKind::Def { kind } => match kind {
                        DefKind::Function { .. } => {
                            let func_id = func_ids[i];
                            if let Err(e) = lower_fn(def_node, &mut info, func_id) {
                                info.diagnostics.push(e.into_cloneable());
                                had_error = true;
                                continue;
                            }

                            // compile just for asm
                            let mut ctrl_plane = ControlPlane::default();
                            if let Err(e) = info.ctx.compile(info.module.isa(), &mut ctrl_plane) {
                                eprintln!("!!! cranelift compile error: {:?}", e);
                                return Err(crate::codegen_err!(
                                    "Cranelift compile error: {:?}",
                                    Some(def_node.span.to_display(interner)),
                                    e
                                ));
                            }

                            // store assembly before define_function consumes it
                            let assembly = info.ctx.compiled_code().and_then(|code| code.vcode.clone());

                            if let Err(e) = info.module.define_function(func_id, &mut info.ctx) {
                                eprintln!("!!! cranelift error: {:?}", e);
                                return Err(crate::codegen_err!(
                                    "Cranelift module error: {:?}",
                                    Some(def_node.span.to_display(interner)),
                                    e
                                ));
                            }

                            if let Some(disasm) = assembly {
                                println!("------------------------------");
                                println!("\n*** Assembly:\n{}", disasm);
                                println!("==============================");
                            }

                            info.ctx.clear();
                            info.ctx.set_disasm(true);
                        }
                        _ => {
                            return Err(crate::codegen_err!(
                                "Unsupported non-function definition: only function definitions are currently supported",
                                Some(def_node.span.to_display(interner))
                            )
                            .attach(Help("This feature is not yet implemented".into())));
                        }
                    },
                    _ => {
                        return Err(crate::codegen_err!(
                            "Invalid node: only definition nodes are currently supported",
                            Some(def_node.span.to_display(interner))
                        )
                        .attach(Help("This construct is not valid in the current context".into())));
                    }
                }
            }

            if had_error {
                return Ok(info.diagnostics);
            }
        }
        _ => {
            return Err(
                crate::codegen_err!("Invalid node: expected root node in codegen", Some(root.span.to_display(interner)))
                    .attach(Help("This construct is not valid in the current context".into())),
            );
        }
    }

    let product = info.module.finish();
    let obj_bytes = product
        .emit()
        .map_err(|e| crate::codegen_err!("Object emit error: {:?}", None, e))?;
    std::fs::write("vtlib.o", obj_bytes).expect("Couldn't write");
    println!("Object file 'vtlib.o' emitted.");
    Ok(info.diagnostics)
}

fn isa() -> (Arc<dyn TargetIsa + 'static>, CallConv) {
    let triple = Triple::host();

    let flag_builder = settings::builder();
    let flags = Flags::new(flag_builder);

    match isa::lookup(triple.clone()) {
        Ok(isa_builder) => match isa_builder.finish(flags) {
            Ok(isa) => {
                let call_conv = isa.default_call_conv();
                (isa, call_conv)
            }
            Err(e) => panic!("failed to create ISA for {}: {}", triple, e),
        },
        Err(e) => panic!("target {} is not supported: {}", triple, e),
    }
}
