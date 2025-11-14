use cranelift::codegen::{CodegenError, CompileError};
use cranelift::module::ModuleError;
use cranelift::object;

use crate::compiler::parser::node::VType;
use crate::compiler::tokens::DisplaySpan;
use std::fmt::Display;

crate::define_errors! {
    TranslateError, phase: Codegen {
        #[msg = "Unexpected {what}"]
        #[help = "This should not occur during code generation"]
        Unexpected {
            what: String,
            context: String,
            span: DisplaySpan,
        },

        #[msg = "Expected {what}, got {got}"]
        #[help = "The code generator expected a different construct here"]
        Expected {
            what: String,
            got: String,
            span: DisplaySpan,
        },

        #[msg = "Invalid {what}: {reason}"]
        #[help = "This construct cannot be translated to machine code"]
        Invalid {
            what: String,
            reason: String,
            span: DisplaySpan,
        },

        #[msg = "Not found: {what}"]
        #[help = "This identifier or construct was not found during code generation"]
        NotFound {
            what: String,
            span: DisplaySpan,
        },

        #[msg = "Unsupported {what}"]
        #[help = "This feature is not yet implemented in the code generator"]
        Unsupported {
            what: String,
            reason: String,
            span: DisplaySpan,
        },

        #[msg = "Type error: {message}"]
        #[help = "There was a type mismatch during code generation"]
        TypeError {
            message: String,
            span: DisplaySpan,
        },

        #[msg = "External error: {message}"]
        #[help = "An error occurred in an external library (Cranelift or object file writer)"]
        External {
            message: String,
            span: DisplaySpan,
        },
    }
}

// Keep the From impls for external error types
impl From<CodegenError> for TranslateError {
    fn from(err: CodegenError) -> Self {
        // Use a dummy span since cranelift errors don't have source locations
        TranslateError::External {
            message: format!("Cranelift codegen error: {}", err),
            span: DisplaySpan {
                file: "<codegen>".to_string(),
                start: (0, 0),
                end: (0, 0),
            },
        }
    }
}

impl From<CompileError<'_>> for TranslateError {
    fn from(err: CompileError) -> Self {
        TranslateError::External {
            message: format!("Cranelift compile error: {}", err.inner),
            span: DisplaySpan {
                file: "<codegen>".to_string(),
                start: (0, 0),
                end: (0, 0),
            },
        }
    }
}

impl From<ModuleError> for TranslateError {
    fn from(err: ModuleError) -> Self {
        TranslateError::External {
            message: format!("Cranelift module error: {}", err),
            span: DisplaySpan {
                file: "<codegen>".to_string(),
                start: (0, 0),
                end: (0, 0),
            },
        }
    }
}

impl From<object::object::write::Error> for TranslateError {
    fn from(err: object::object::write::Error) -> Self {
        TranslateError::External {
            message: format!("Object write error: {}", err),
            span: DisplaySpan {
                file: "<codegen>".to_string(),
                start: (0, 0),
                end: (0, 0),
            },
        }
    }
}

impl Display for VType {
    // TODO: Actually impl Display for Type
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}
