//! Type conversion utilities between Volette types and Citadel IR types

use citadel_api::frontend::ir::{FLOAT32_T, FLOAT64_T, INT8_T, INT16_T, INT32_T, INT64_T, NEVER_T, Type, UNIT_T};

use crate::compiler::{parser::node::VType, tokens::PrimitiveTypes};

impl VType {
    pub fn to_citadel_type(&self) -> Type<'static> {
        match self {
            VType::Primitive(p) => Type::Ident(p.to_citadel_str()),
            VType::Pointer(_) => Type::Ident(INT64_T),
            VType::Custom(_) => panic!("Custom types not yet supported in Citadel backend"),
        }
    }

    pub fn is_zst(&self) -> bool {
        matches!(
            self,
            VType::Primitive(PrimitiveTypes::Unit) | VType::Primitive(PrimitiveTypes::Never)
        )
    }
}

impl PrimitiveTypes {
    pub fn to_citadel_str(&self) -> &'static str {
        match self {
            PrimitiveTypes::I8 => INT8_T,
            PrimitiveTypes::U8 => INT8_T,
            PrimitiveTypes::I16 => INT16_T,
            PrimitiveTypes::U16 => INT16_T,
            PrimitiveTypes::I32 => INT32_T,
            PrimitiveTypes::U32 => INT32_T,
            PrimitiveTypes::I64 => INT64_T,
            PrimitiveTypes::U64 => INT64_T,
            PrimitiveTypes::Isize => INT64_T,
            PrimitiveTypes::Usize => INT64_T,
            PrimitiveTypes::F32 => FLOAT32_T,
            PrimitiveTypes::F64 => FLOAT64_T,
            PrimitiveTypes::Bool => INT8_T,
            PrimitiveTypes::Unit => UNIT_T,
            PrimitiveTypes::Never => NEVER_T,
        }
    }

    pub fn is_zst(&self) -> bool {
        matches!(self, PrimitiveTypes::Unit | PrimitiveTypes::Never)
    }
}
