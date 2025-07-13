use std::fmt::Display;

use cranelift::codegen::CodegenError;
use cranelift::module::ModuleError;
use cranelift::object;
use thiserror::Error;

use crate::compiler::parser::node::{Node, Type};
use crate::compiler::tokens::DisplaySpan;

#[derive(Error, Debug)]
pub enum TranslateError {
    #[error("Cranelift Codegen Error: {0}")]
    CodegenError(#[from] CodegenError),

    #[error("Cranelift Module Error: {0}")]
    ModuleError(#[from] ModuleError),

    #[error("Expected block expression: {span}")]
    ExpectedBlockExpression { span: DisplaySpan },

    #[error("Object Write Error: {0}")]
    ObjectWriteError(#[from] object::object::write::Error),

    #[error("Incorrect type hit. Expected {type_} but got {node}, which cannot be coerced into {type_}.")]
    IncorrectTypeAnalysis { type_: Type, node: Node },
}

impl Display for Type {
    // TODO: Actually impl Display for Type
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}
