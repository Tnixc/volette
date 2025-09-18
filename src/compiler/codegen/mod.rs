use std::{collections::HashMap, str::FromStr, sync::Arc};

use cranelift::{
    codegen::Context,
    module::{Module, default_libcall_names, Linkage},
    object::{self, ObjectModule},
    prelude::{
        isa::{self, CallConv, TargetIsa},
        settings::{self, Flags},
        AbiParam, Signature,
    },
};
use generational_arena::Arena;
use string_interner::{StringInterner, backend::BucketBackend, symbol::SymbolUsize};
use target_lexicon::Triple;

use crate::compiler::{
    codegen::{error::TranslateError, function::lower_fn},
    parser::node::{DefKind, Node, NodeKind, Type},
};

pub mod error;
pub mod expr;
pub mod function;
pub mod translate;

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
    let target_isa = isa();
    let builder = object::ObjectBuilder::new(target_isa, "vtlib", default_libcall_names())?;

    let module = ObjectModule::new(builder);
    let ctx = Context::new();

    let build_config = BuildConfig {
        ptr_width: PtrWidth::X64,
        call_conv: CallConv::SystemV,
    };

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
                let def_node = nodes.get(*def).expect("A definition node was not found");
                match &def_node.kind {
                    NodeKind::Def { kind } => match kind {
                        DefKind::Function { name, params, return_type, .. } => {
                            let fn_name = info.interner.resolve(*name).expect("Function name's string not found in interner");
                            
                            let mut sig = Signature::new(info.build_config.call_conv);
                            for param in params {
                                sig.params.push(AbiParam::new(param.1.to_clif(info.build_config.ptr_width)));
                            }
                            sig.returns.push(AbiParam::new(return_type.to_clif(info.build_config.ptr_width)));
                            
                            let func_id = info.module.declare_function(fn_name, Linkage::Export, &sig)?;
                            func_ids.push(func_id);
                            info.func_table.insert(*name, func_id);
                        }
                        _ => todo!("only functions are supported rn"),
                    },
                    _ => todo!("only functions are supported rn"),
                }
            }
            
            // define function bodies
            for (i, def) in defs.iter().enumerate() {
                let def_node = nodes.get(*def).expect("A definition node was not found");
                match &def_node.kind {
                    NodeKind::Def { kind } => match kind {
                        DefKind::Function { .. } => {
                            let func_id = func_ids[i];
                            lower_fn(def_node, &mut info, func_id)?;
                            info.module.define_function(func_id, &mut info.ctx)?;
                        }
                        _ => todo!("only functions are supported rn"),
                    },
                    _ => todo!("only functions are supported rn"),
                }
            }
        }
        _ => unreachable!(),
    }

    let product = info.module.finish();
    let obj_bytes = product.emit()?;

    std::fs::write("vtlib.o", obj_bytes).expect("Couldn't write");
    println!("Object file 'vtlib.o' emitted.");
    Ok(())
}

fn isa() -> Arc<dyn TargetIsa + 'static> {
    let triple_str = "aarch64-apple-darwin";
    let triple = Triple::from_str(triple_str).unwrap();

    let flag_builder = settings::builder();
    let flags = Flags::new(flag_builder);

    let isa_builder = isa::lookup(triple.clone()).unwrap();

    (isa_builder.finish(flags).unwrap()) as _
}
