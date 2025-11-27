pub mod codegen;
pub mod elements;

use citadel_frontend::ir::irgen::HIRStream;

use crate::api::{Backend, Target};
use elements::AArch64Element;

#[derive(Debug, Default, Clone, Copy)]
pub struct TargetAArch64;

impl Target for TargetAArch64 {
    fn name(&self) -> &str {
        "aarch64"
    }
}

#[derive(Debug, Default)]
pub struct AArch64Backend<T: Target> {
    target: T,
}

impl<T: Target> AArch64Backend<T> {
    pub fn new(target: T) -> Self {
        Self { target }
    }
}

impl<T: Target> Backend for AArch64Backend<T> {
    type Output = Vec<AArch64Element>;
    type Target = T;

    fn target(&self) -> Self::Target {
        self.target
    }

    fn generate(&self, ir_stream: HIRStream) -> Self::Output {
        codegen::compile_program(ir_stream)
    }

    fn format(&self, output: &Self::Output) -> Option<String> {
        Some(codegen::format(output.as_slice()))
    }
}
