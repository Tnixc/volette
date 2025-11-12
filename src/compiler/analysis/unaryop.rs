use std::collections::HashMap;

use generational_arena::{Arena, Index};
use string_interner::{StringInterner, backend::BucketBackend, symbol::SymbolUsize};

use crate::{
    SafeConvert,
    compiler::{
        analysis::type_check::resolve_expr_type,
        parser::node::{Node, Type, UnaryOpKind},
        tokens::PrimitiveTypes,
    },
};

use super::error::AnalysisError;

pub(crate) fn check_unaryop(
    expr: Index,
    op: UnaryOpKind,
    nodes: &mut Arena<Node>,
    interner: &StringInterner<BucketBackend<SymbolUsize>>,
    ident_types: &mut HashMap<SymbolUsize, Type>,
    fn_table: &HashMap<SymbolUsize, (Box<Vec<Type>>, Type)>,
) -> Result<Type, AnalysisError> {
    let expr_ty = resolve_expr_type(expr, None, nodes, interner, ident_types, fn_table)?;
    let expr_span = nodes.get(expr).safe().span;

    match op {
        UnaryOpKind::Neg => match &expr_ty {
            Type::Primitive(pt) if pt.is_integer() || pt.is_float() => Ok(expr_ty),
            _ => Err(AnalysisError::Invalid {
                what: "negation operator".to_string(),
                reason: format!("cannot negate type {:?}, expected numeric type", expr_ty),
                span: expr_span.to_display(interner),
            }),
        },
        UnaryOpKind::Not => {
            if expr_ty != Type::Primitive(PrimitiveTypes::Bool) {
                return Err(AnalysisError::TypeMismatch {
                    expected: format!("{:?}", Type::Primitive(PrimitiveTypes::Bool)),
                    got: format!("{:?}", expr_ty),
                    span: expr_span.to_display(interner),
                });
            }
            Ok(Type::Primitive(PrimitiveTypes::Bool))
        }
        UnaryOpKind::Deref => match expr_ty {
            Type::Pointer(inner_type) => Ok(*inner_type),
            _ => Err(AnalysisError::Invalid {
                what: "dereference operator".to_string(),
                reason: format!("cannot dereference type {:?}, expected pointer type", expr_ty),
                span: expr_span.to_display(interner),
            }),
        },
        UnaryOpKind::AddressOf => {
            // any
            Ok(Type::Pointer(Box::new(expr_ty)))
        }
    }
}
