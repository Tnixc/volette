use thiserror::Error;

use crate::compiler::{
    parser::node::{BinOpKind, Type},
    tokens::DisplaySpan,
};

#[derive(Debug, Error, Clone)]
pub enum AnalysisError {
    #[error("Type mismatch at {span}: expected {expected}, got {got}")]
    TypeMismatch { expected: Type, got: Type, span: DisplaySpan },

    #[error("Unresolved identifier '{name}' at {span}")]
    UnresolvedIdentifier { name: String, span: DisplaySpan },

    #[error("Internal error: {0}")]
    Internal(String),

    #[error("Invalid binary operation at {span}: {op}")]
    _InvalidBinOp { op: BinOpKind, ty: Type, span: DisplaySpan },

    #[error("Undeclared function '{name}' at {span}")]
    UndeclaredFunction { name: String, span: DisplaySpan },
}

impl AnalysisError {
    pub fn span(&self) -> Option<&DisplaySpan> {
        match self {
            AnalysisError::TypeMismatch { span, .. } => Some(span),
            AnalysisError::UnresolvedIdentifier { span, .. } => Some(span),
            AnalysisError::Internal(_) => None,
            AnalysisError::_InvalidBinOp { span, .. } => Some(span),
            AnalysisError::UndeclaredFunction { span, .. } => Some(span),
        }
    }
}
