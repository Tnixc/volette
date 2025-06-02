use cranelift::codegen::CodegenError;
use cranelift::module::ModuleError;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum CraneliftError {
    #[error("Cranelift Codegen Error: {0}")]
    CodegenError(#[from] CodegenError),

    #[error("Cranelift Module Error: {0}")]
    ModuleError(#[from] ModuleError),
}
