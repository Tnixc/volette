use crate::{
    compiler::{parser::node::VType, tokens::PrimitiveTypes},
    is_float, is_int,
};

pub fn can_convert(from: &VType, to: &VType) -> bool {
    match (from, to) {
        (VType::Primitive(from), VType::Primitive(to)) => {
            matches!((from, to), (is_int!() | is_float!(), is_int!() | is_float!()))
                | matches!((from, to), (PrimitiveTypes::Bool, is_int!()))
        }
        (VType::Pointer(_any), VType::Primitive(to)) => matches!(to, PrimitiveTypes::Usize | PrimitiveTypes::Isize),
        (VType::Primitive(from), VType::Pointer(_any)) => matches!(from, PrimitiveTypes::Usize | PrimitiveTypes::Isize),
        _ => false,
    }
}
