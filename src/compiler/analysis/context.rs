use std::collections::HashMap;

use generational_arena::Arena;
use string_interner::{StringInterner, backend::BucketBackend, symbol::SymbolUsize};

use crate::compiler::{
    error::ReportCollection,
    parser::node::{Node, VType},
};

pub struct TypeCtx<'a> {
    pub nodes: &'a mut Arena<Node>,
    pub interner: &'a StringInterner<BucketBackend<SymbolUsize>>,
    pub ident_types: HashMap<SymbolUsize, VType>,
    pub fn_table: &'a HashMap<SymbolUsize, (Vec<VType>, VType)>,
    pub diagnostics: &'a mut ReportCollection,
}

impl<'a> TypeCtx<'a> {
    pub fn new(
        nodes: &'a mut Arena<Node>,
        interner: &'a StringInterner<BucketBackend<SymbolUsize>>,
        fn_table: &'a HashMap<SymbolUsize, (Vec<VType>, VType)>,
        diagnostics: &'a mut ReportCollection,
    ) -> Self {
        Self {
            nodes,
            interner,
            ident_types: HashMap::new(),
            fn_table,
            diagnostics,
        }
    }
}
