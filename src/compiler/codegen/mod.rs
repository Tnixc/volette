use std::{str::FromStr, sync::Arc};

use cranelift::{
    codegen::Context,
    module::default_libcall_names,
    object::{self, ObjectModule},
    prelude::{
        isa::{self, CallConv, TargetIsa},
        settings::{self, Flags},
    },
};
use generational_arena::Arena;
use string_interner::{backend::BucketBackend, symbol::SymbolUsize, StringInterner};
use target_lexicon::Triple;

use crate::compiler::{
    codegen::{error::TranslateError, function::lower_fn},
    parser::node::{DefKind, Node, NodeKind},
};

pub mod block;
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
}

pub fn codegen(
    root: &Node,
    nodes: &Arena<Node>,
    interner: &StringInterner<BucketBackend<SymbolUsize>>,
) -> Result<(), TranslateError> {
    let target_isa = isa();
    let builder = object::ObjectBuilder::new(target_isa, "vtlib", default_libcall_names())?;

    let mut module = ObjectModule::new(builder);
    let mut ctx = Context::new();

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
    };

    match &root.kind {
        NodeKind::Root { defs } => {
            for def in defs {
                let def_node = nodes.get(*def).expect("A defintion node was not found");
                match &def_node.kind {
                    NodeKind::Def { kind } => match kind {
                        DefKind::Function { .. } => {
                            lower_fn(def_node, &mut info)?;
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
    let target_isa = isa_builder.finish(flags).unwrap();
    target_isa
}
