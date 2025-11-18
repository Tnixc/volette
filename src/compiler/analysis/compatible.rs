use crate::compiler::{parser::node::VType, tokens::PrimitiveTypes};

pub fn can_convert(from: &VType, to: &VType) -> bool {
    match (from, to) {
        (VType::Primitive(from), VType::Primitive(to)) => {
            (from.is_int() || from.is_float()) && (to.is_int() || to.is_float()) // num to num
                || (from == &PrimitiveTypes::Bool && to.is_int()) // bool to int
        }
        (VType::Pointer(_), VType::Primitive(to)) => matches!(to, PrimitiveTypes::Usize | PrimitiveTypes::Isize),
        (VType::Primitive(from), VType::Pointer(_)) => matches!(from, PrimitiveTypes::Usize | PrimitiveTypes::Isize),
        _ => false,
    }
}
