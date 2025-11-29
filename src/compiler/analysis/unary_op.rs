use std::collections::HashMap;

use generational_arena::{Arena, Index};
use rootcause::prelude::*;
use string_interner::{StringInterner, backend::BucketBackend, symbol::SymbolUsize};

use crate::{
    SafeConvert,
    compiler::{
        analysis::type_check::resolve_expr_type,
        error::{Help, ReportCollection},
        parser::node::{Node, UnaryOpKind, VType},
        tokens::PrimitiveTypes,
    },
    is_float, is_int,
};

pub fn check_unary_op(
    expr: Index,
    op: UnaryOpKind,
    nodes: &mut Arena<Node>,
    interner: &StringInterner<BucketBackend<SymbolUsize>>,
    ident_types: &mut HashMap<SymbolUsize, VType>,
    fn_table: &HashMap<SymbolUsize, (Box<Vec<VType>>, VType)>,
    diagnostics: &mut ReportCollection,
) -> Result<VType, Report> {
    let expr_ty = resolve_expr_type(expr, None, nodes, interner, ident_types, fn_table, diagnostics)?;
    let expr_span = nodes.get(expr).safe().span;

    match op {
        UnaryOpKind::Neg => match &expr_ty {
            VType::Primitive(is_float!() | is_int!()) => Ok(expr_ty),
            _ => Err(crate::analysis_err!(
                "Invalid negation operator: cannot negate type {:?}, expected numeric type",
                Some(expr_span.to_display(interner)),
                expr_ty
            )
            .attach(Help("This construct is not valid in the current context".into()))),
        },
        UnaryOpKind::Not => {
            if expr_ty != VType::Primitive(PrimitiveTypes::Bool) {
                return Err(crate::analysis_err!(
                    "Type mismatch: expected {:?}, got {:?}",
                    Some(expr_span.to_display(interner)),
                    VType::Primitive(PrimitiveTypes::Bool),
                    expr_ty
                )
                .attach(Help("Ensure the types match or add an explicit conversion".into())));
            }
            Ok(VType::Primitive(PrimitiveTypes::Bool))
        }
        UnaryOpKind::Deref => match expr_ty {
            VType::Pointer(inner_type) => Ok(*inner_type),
            _ => Err(crate::analysis_err!(
                "Invalid dereference operator: cannot dereference type {:?}, expected pointer type",
                Some(expr_span.to_display(interner)),
                expr_ty
            )
            .attach(Help("This construct is not valid in the current context".into()))),
        },
        UnaryOpKind::AddressOf => {
            // any
            Ok(VType::Pointer(Box::new(expr_ty)))
        }
    }
}
