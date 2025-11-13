use std::collections::HashMap;

use generational_arena::{Arena, Index};
use string_interner::{StringInterner, backend::BucketBackend, symbol::SymbolUsize};

use crate::{
    SafeConvert,
    compiler::{
        analysis::type_check::resolve_expr_type,
        parser::node::{Node, UnaryOpKind, VType},
        tokens::PrimitiveTypes,
    },
    is_float, is_int,
};

use super::error::AnalysisError;

pub(crate) fn check_unaryop(
    expr: Index,
    op: UnaryOpKind,
    nodes: &mut Arena<Node>,
    interner: &StringInterner<BucketBackend<SymbolUsize>>,
    ident_types: &mut HashMap<SymbolUsize, VType>,
    fn_table: &HashMap<SymbolUsize, (Box<Vec<VType>>, VType)>,
) -> Result<VType, AnalysisError> {
    let expr_ty = resolve_expr_type(expr, None, nodes, interner, ident_types, fn_table)?;
    let expr_span = nodes.get(expr).safe().span;

    match op {
        UnaryOpKind::Neg => match &expr_ty {
            VType::Primitive(is_float!() | is_int!()) => Ok(expr_ty),
            _ => Err(AnalysisError::Invalid {
                what: "negation operator".to_string(),
                reason: format!("cannot negate type {:?}, expected numeric type", expr_ty),
                span: expr_span.to_display(interner),
            }),
        },
        UnaryOpKind::Not => {
            if expr_ty != VType::Primitive(PrimitiveTypes::Bool) {
                return Err(AnalysisError::TypeMismatch {
                    expected: format!("{:?}", VType::Primitive(PrimitiveTypes::Bool)),
                    got: format!("{:?}", expr_ty),
                    span: expr_span.to_display(interner),
                });
            }
            Ok(VType::Primitive(PrimitiveTypes::Bool))
        }
        UnaryOpKind::Deref => match expr_ty {
            VType::Pointer(inner_type) => Ok(*inner_type),
            _ => Err(AnalysisError::Invalid {
                what: "dereference operator".to_string(),
                reason: format!("cannot dereference type {:?}, expected pointer type", expr_ty),
                span: expr_span.to_display(interner),
            }),
        },
        UnaryOpKind::AddressOf => {
            // any
            Ok(VType::Pointer(Box::new(expr_ty)))
        }
    }
}
