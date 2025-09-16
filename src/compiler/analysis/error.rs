use thiserror::Error;

use crate::compiler::{
    parser::node::{BinOpKind, Type},
    tokens::DisplaySpan,
};

#[derive(Debug, Error)]
pub enum AnalysisError {
    #[error("Type mismatch at {span}: expected {expected}, got {got}")]
    TypeMismatch { expected: Type, got: Type, span: DisplaySpan },

    #[error("Unresolved identifier '{name}' at {span}")]
    UnresolvedIdentifier { name: String, span: DisplaySpan },

    #[error("Internal error: {0}")]
    Internal(String),

    #[error("Invalid binary operation at {span}: {op}")]
    InvalidBinOp { op: BinOpKind, ty: Type, span: DisplaySpan },
}
