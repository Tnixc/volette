use thiserror::Error;

use crate::compiler::{parser::node::Type, tokens::DisplaySpan};

#[derive(Debug, Error)]
pub enum AnalysisError {
    #[error("Type mismatch at {span}: expected {expected}, got {got}")]
    TypeMismatch {
        expected: Type,
        got: Type,
        span: DisplaySpan,
    },

    #[error("Unresolved identifier '{name}' at {span}")]
    UnresolvedIdentifier { name: String, span: DisplaySpan },
}
