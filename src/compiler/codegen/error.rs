use std::fmt::Display;

use cranelift::codegen::CodegenError;
use cranelift::module::ModuleError;
use cranelift::object;
use thiserror::Error;

use crate::compiler::parser::node::{Node, Type};
use crate::compiler::tokens::DisplaySpan;

#[derive(Error, Debug, Clone)]
pub enum TranslateError {
    #[error("Cranelift Codegen Error: {0}")]
    CodegenError(String),

    #[error("Cranelift Module Error: {0}")]
    ModuleError(String),

    #[error("Expected block expression: {span}")]
    ExpectedBlockExpression { span: DisplaySpan },

    #[error("Object Write Error: {0}")]
    ObjectWriteError(String),

    #[error("Incorrect type hit. Expected {type_} but got {node}, which cannot be coerced into {type_}.")]
    IncorrectTypeAnalysis { type_: Type, node: Node },

    #[error("No type information available for {node}")]
    UntypedExpr { node: Node },

    #[error("Internal error: {0}")]
    Internal(String),

    #[error("Undeclared identifier: {0}")]
    UndeclaredIdentifier(String),
}

impl From<CodegenError> for TranslateError {
    fn from(err: CodegenError) -> Self {
        TranslateError::CodegenError(err.to_string())
    }
}

impl From<ModuleError> for TranslateError {
    fn from(err: ModuleError) -> Self {
        TranslateError::ModuleError(err.to_string())
    }
}

impl From<object::object::write::Error> for TranslateError {
    fn from(err: object::object::write::Error) -> Self {
        TranslateError::ObjectWriteError(err.to_string())
    }
}

impl TranslateError {
    pub fn span(&self) -> Option<&DisplaySpan> {
        match self {
            TranslateError::CodegenError(_) => None,
            TranslateError::ModuleError(_) => None,
            TranslateError::ExpectedBlockExpression { span } => Some(span),
            TranslateError::ObjectWriteError(_) => None,
            TranslateError::IncorrectTypeAnalysis { .. } => None, // Node doesn't have direct span access
            TranslateError::UntypedExpr { .. } => None,           // Node doesn't have direct span access
            TranslateError::Internal(_) => None,
            TranslateError::UndeclaredIdentifier(_) => None,
        }
    }
}

impl Display for Type {
    // TODO: Actually impl Display for Type
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}
