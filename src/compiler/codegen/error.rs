use cranelift::codegen::CodegenError;
use cranelift::module::ModuleError;
use thiserror::Error;

use crate::compiler::tokens::DisplaySpan;

#[derive(Error, Debug)]
pub enum TranslateError {
    #[error("Cranelift Codegen Error: {0}")]
    CodegenError(#[from] CodegenError),

    #[error("Cranelift Module Error: {0}")]
    ModuleError(#[from] ModuleError),

    #[error("Expected block expression: {span}")]
    ExpectedBlockExpression { span: DisplaySpan },
}
