use cranelift::prelude::isa::CallConv;

pub mod error;
pub mod function;
pub mod translate;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum PtrWidth {
    X32,
    X64,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct BuildConfig {
    pub ptr_width: PtrWidth,
    pub call_conv: CallConv,
}
