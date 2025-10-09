use cranelift::prelude::types;

use crate::compiler::{codegen::PtrWidth, tokens::PrimitiveTypes};

impl PrimitiveTypes {
    pub fn to_clif(&self, ptr_bits: PtrWidth) -> types::Type {
        use PrimitiveTypes::*;
        match self {
            I8 => types::I8,
            U8 => types::I8,
            I16 => types::I16,
            U16 => types::I16,
            I32 => types::I32,
            U32 => types::I32,
            I64 => types::I64,
            U64 => types::I64,
            Isize | Usize => match ptr_bits {
                PtrWidth::X32 => types::I32,
                PtrWidth::X64 => types::I64,
            },
            F32 => types::F32,
            F64 => types::F64,
            Bool => types::I8,
            Nil | Never => types::INVALID,
        }
    }
}
