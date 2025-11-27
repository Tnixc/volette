//! Citadel backend for Volette

mod lower;
mod types;

use std::path::PathBuf;

use bumpalo::Bump;
use citadel_api::backend::aarch64::{AArch64Backend, TargetAArch64};
use citadel_api::backend::api::Backend;
use generational_arena::Arena;
use rootcause::prelude::*;
use string_interner::{StringInterner, backend::BucketBackend, symbol::SymbolUsize};

use crate::compiler::{error::ReportCollection, parser::node::Node};

use lower::CitadelLowering;

pub fn codegen(
    root: &Node,
    nodes: &Arena<Node>,
    interner: &StringInterner<BucketBackend<SymbolUsize>>,
) -> Result<ReportCollection, Report> {
    let arena = Bump::new();
    let diagnostics = ReportCollection::new();

    let lowering = CitadelLowering::new(&arena, nodes, interner);
    let hir = lowering.lower(root)?;

    println!("=== Citadel HIR ===");
    println!("{}", hir);
    println!("===================");

    let backend = AArch64Backend::new(TargetAArch64);
    let output = backend.generate(hir);
    let asm_output = backend.format(&output).unwrap_or_default();

    let asm_path = PathBuf::from("output.s");
    std::fs::write(&asm_path, &asm_output).map_err(|e| crate::codegen_err!("Failed to write assembly file: {}", None, e))?;

    println!("Assembly written to: {}", asm_path.display());

    Ok(diagnostics)
}
