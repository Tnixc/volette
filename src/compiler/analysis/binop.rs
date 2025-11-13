use std::collections::HashMap;

use generational_arena::{Arena, Index};
use string_interner::{StringInterner, backend::BucketBackend, symbol::SymbolUsize};

use crate::{
    SafeConvert,
    compiler::{
        analysis::type_check::resolve_expr_type,
        parser::node::{BinOpKind, Node, VType},
        tokens::PrimitiveTypes,
    },
};

use super::error::AnalysisError;

pub(crate) fn check_binop(
    left: Index,
    right: Index,
    op: BinOpKind,
    nodes: &mut Arena<Node>,
    interner: &StringInterner<BucketBackend<SymbolUsize>>,
    ident_types: &mut HashMap<SymbolUsize, VType>,
    fn_table: &HashMap<SymbolUsize, (Box<Vec<VType>>, VType)>,
) -> Result<VType, AnalysisError> {
    let left_ty = resolve_expr_type(left, None, nodes, interner, ident_types, fn_table)?;
    let right_ty = resolve_expr_type(right, None, nodes, interner, ident_types, fn_table)?;

    if left_ty == VType::Primitive(PrimitiveTypes::Nil) || left_ty == VType::Primitive(PrimitiveTypes::Nil) {
        return Err(AnalysisError::Invalid {
            what: format!("binary operation '{:?}'", op),
            reason: format!("cannot be used with type {:?}", left_ty),
            span: nodes
                .get(left)
                .safe()
                .span
                .connect_new(&nodes.get(right).safe().span)
                .to_display(interner),
        });
    }

    if left_ty.coerced() != right_ty.coerced() {
        return Err(AnalysisError::TypeMismatch {
            expected: format!("{:?}", left_ty),
            got: format!("{:?}", right_ty),
            span: nodes.get(right).safe().span.to_display(interner),
        });
    }

    match op {
        BinOpKind::Add | BinOpKind::Sub | BinOpKind::Mul | BinOpKind::Div | BinOpKind::Mod => {
            // TODO??
            Ok(left_ty)
        }
        BinOpKind::BitwiseAnd | BinOpKind::BitwiseOr | BinOpKind::BitwiseXor => {
            // Bitwise operators return the same type as operands
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
        _ => Err(AnalysisError::Unsupported {
            what: format!("binary operator '{:?}'", op),
            span: nodes
                .get(left)
                .safe()
                .span
                .connect_new(&nodes.get(right).safe().span)
                .to_display(interner),
        }),
    }
}
