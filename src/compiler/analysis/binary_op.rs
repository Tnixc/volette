use std::collections::HashMap;

use generational_arena::{Arena, Index};
use rootcause::prelude::*;
use string_interner::{StringInterner, backend::BucketBackend, symbol::SymbolUsize};

use crate::{
    SafeConvert,
    compiler::{
        analysis::type_check::resolve_expr_type,
        error::{Help, ReportCollection},
        parser::node::{BinOpKind, Node, VType},
        tokens::PrimitiveTypes,
    },
};

pub fn check_binary_op(
    left: Index,
    right: Index,
    op: BinOpKind,
    nodes: &mut Arena<Node>,
    interner: &StringInterner<BucketBackend<SymbolUsize>>,
    ident_types: &mut HashMap<SymbolUsize, VType>,
    fn_table: &HashMap<SymbolUsize, (Box<Vec<VType>>, VType)>,
    diagnostics: &mut ReportCollection,
) -> Result<VType, Report> {
    let left_ty = resolve_expr_type(left, None, nodes, interner, ident_types, fn_table, diagnostics)?;
    let right_ty = resolve_expr_type(right, None, nodes, interner, ident_types, fn_table, diagnostics)?;

    if left_ty == VType::Primitive(PrimitiveTypes::Unit) || right_ty == VType::Primitive(PrimitiveTypes::Unit) {
        return Err(crate::analysis_err!(
            "Invalid binary operation '{:?}': cannot be used with type {:?}",
            Some(
                nodes
                    .get(left)
                    .safe()
                    .span
                    .connect_new(&nodes.get(right).safe().span)
                    .to_display(interner),
            ),
            op,
            left_ty
        )
        .attach(Help("This construct is not valid in the current context".into())));
    }

    if left_ty.coerced() != right_ty.coerced() {
        return Err(crate::analysis_err!(
            "Type mismatch: expected {:?}, got {:?}",
            Some(nodes.get(right).safe().span.to_display(interner)),
            left_ty,
            right_ty
        )
        .attach(Help("Ensure the types match or add an explicit conversion".into())));
    }

    match op {
        BinOpKind::Add | BinOpKind::Sub | BinOpKind::Mul | BinOpKind::Div | BinOpKind::Mod => {
            // TODO??
            Ok(left_ty)
        }
        BinOpKind::BitwiseAnd | BinOpKind::BitwiseOr | BinOpKind::BitwiseXor | BinOpKind::BitwiseShLeft | BinOpKind::BitwiseShRight => {
            Ok(left_ty)
        }
        BinOpKind::Eq
        | BinOpKind::NotEq
        | BinOpKind::LessThan
        | BinOpKind::GreaterThanOrEq
        | BinOpKind::GreaterThan
        | BinOpKind::LessThanOrEq
        | BinOpKind::LogicalAnd
        | BinOpKind::LogicalOr => Ok(VType::Primitive(PrimitiveTypes::Bool)),
        _ => Err(crate::analysis_err!(
            "Unsupported binary operator '{:?}'",
            Some(
                nodes
                    .get(left)
                    .safe()
                    .span
                    .connect_new(&nodes.get(right).safe().span)
                    .to_display(interner),
            ),
            op
        )
        .attach(Help("This feature is not yet implemented".into()))),
    }
}
