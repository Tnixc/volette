//! Code generation module for Volette

pub mod citadel;

use std::path::Path;

use generational_arena::Arena;
use rootcause::prelude::*;
use string_interner::{StringInterner, backend::BucketBackend, symbol::SymbolUsize};

use crate::compiler::{error::ReportCollection, parser::node::Node};

pub fn codegen(
    root: &Node,
    nodes: &Arena<Node>,
    interner: &StringInterner<BucketBackend<SymbolUsize>>,
    output_path: &Path,
) -> Result<ReportCollection, Report> {
    citadel::codegen(root, nodes, interner, output_path)
}
