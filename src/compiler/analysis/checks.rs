use generational_arena::Index;
use rootcause::prelude::*;

use crate::{
    SafeConvert,
    compiler::{
        analysis::{compatible::can_convert, context::TypeCtx},
        error::Help,
        parser::node::{BinOpKind, Literal, UnaryOpKind, VType},
        tokens::{PrimitiveTypes, Span},
    },
    is_float, is_int,
};

impl<'a> TypeCtx<'a> {
    pub fn check_literal(&self, span: Span, expected: Option<&VType>, literal: Literal) -> Result<VType, Report> {
        let inferred = match literal {
            Literal::Int(_) => VType::Primitive(PrimitiveTypes::I32),
            Literal::Float(_) => VType::Primitive(PrimitiveTypes::F32),
            Literal::Bool(_) => VType::Primitive(PrimitiveTypes::Bool),
        };

        let target = expected.unwrap_or(&inferred);

        if can_convert(&inferred, target) || &inferred == target {
            Ok(target.clone())
        } else {
            Err(crate::analysis_err!(
                "Type mismatch: expected {:?}, got {:?}",
                Some(span.to_display(self.interner)),
                target,
                inferred
            )
            .attach(Help("Ensure the types match or add an explicit conversion".into())))
        }
    }

    pub fn check_binary_op(&mut self, left: Index, right: Index, op: BinOpKind) -> Result<VType, Report> {
        let left_ty = self.resolve_expr_type(left, None)?;
        let right_ty = self.resolve_expr_type(right, None)?;

        if left_ty == VType::Primitive(PrimitiveTypes::Unit) || right_ty == VType::Primitive(PrimitiveTypes::Unit) {
            return Err(crate::analysis_err!(
                "Invalid binary operation '{:?}': cannot be used with type {:?}",
                Some(
                    self.nodes
                        .get(left)
                        .safe()
                        .span
                        .connect_new(&self.nodes.get(right).safe().span)
                        .to_display(self.interner),
                ),
                op,
                left_ty
            )
            .attach(Help("This construct is not valid in the current context".into())));
        }

        if left_ty.coerced() != right_ty.coerced() {
            return Err(crate::analysis_err!(
                "Type mismatch: expected {:?}, got {:?}",
                Some(self.nodes.get(right).safe().span.to_display(self.interner)),
                left_ty,
                right_ty
            )
            .attach(Help("Ensure the types match or add an explicit conversion".into())));
        }

        match op {
            BinOpKind::Add | BinOpKind::Sub | BinOpKind::Mul | BinOpKind::Div | BinOpKind::Mod => Ok(left_ty),
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
                    self.nodes
                        .get(left)
                        .safe()
                        .span
                        .connect_new(&self.nodes.get(right).safe().span)
                        .to_display(self.interner),
                ),
                op
            )
            .attach(Help("This feature is not yet implemented".into()))),
        }
    }

    pub fn check_unary_op(&mut self, expr: Index, op: UnaryOpKind) -> Result<VType, Report> {
        let expr_ty = self.resolve_expr_type(expr, None)?;
        let expr_span = self.nodes.get(expr).safe().span;

        match op {
            UnaryOpKind::Neg => match &expr_ty {
                VType::Primitive(is_float!() | is_int!()) => Ok(expr_ty),
                _ => Err(crate::analysis_err!(
                    "Invalid negation operator: cannot negate type {:?}, expected numeric type",
                    Some(expr_span.to_display(self.interner)),
                    expr_ty
                )
                .attach(Help("This construct is not valid in the current context".into()))),
            },
            UnaryOpKind::Not => {
                if expr_ty != VType::Primitive(PrimitiveTypes::Bool) {
                    return Err(crate::analysis_err!(
                        "Type mismatch: expected {:?}, got {:?}",
                        Some(expr_span.to_display(self.interner)),
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
                    Some(expr_span.to_display(self.interner)),
                    expr_ty
                )
                .attach(Help("This construct is not valid in the current context".into()))),
            },
            UnaryOpKind::AddressOf => Ok(VType::Pointer(Box::new(expr_ty))),
        }
    }
}
