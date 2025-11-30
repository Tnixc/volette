use generational_arena::Index;
use rootcause::prelude::*;

use crate::{
    SafeConvert,
    compiler::{
        analysis::{compatible::can_convert, context::TypeCtx},
        error::Help,
        parser::node::{ExprKind, NodeKind, VType},
        tokens::PrimitiveTypes,
    },
};

impl<'a> TypeCtx<'a> {
    /// Resolves the type of an expression.
    /// If `expected` is `Some`, it checks the expression against that type.
    /// If `expected` is `None`, it infers the expression's type.
    pub fn resolve_expr_type(&mut self, target_idx: Index, expected: Option<&VType>) -> Result<VType, Report> {
        let (kind, span) = {
            let target_node = self.nodes.get(target_idx).safe();
            if let NodeKind::Expr { type_: Some(ty), .. } = &target_node.kind {
                if let Some(expected_ty) = expected
                    && ty != expected_ty
                {
                    return Err(crate::analysis_err!(
                        "Type mismatch: expected {:?}, got {:?}",
                        Some(target_node.span.to_display(self.interner)),
                        expected_ty,
                        ty
                    )
                    .attach(Help("Ensure the types match or add an explicit conversion".into())));
                }
                return Ok(ty.clone());
            }
            (target_node.kind.clone(), target_node.span)
        };

        let inferred_type = match kind {
            NodeKind::Expr { kind, .. } => match kind {
                ExprKind::Identifier(symbol) => self.ident_types.get(&symbol).cloned().ok_or_else(|| {
                    crate::analysis_err!(
                        "Unresolved identifier '{}'",
                        Some(span.to_display(self.interner)),
                        self.interner.resolve(symbol).unwrap_or("<unknown>")
                    )
                    .attach(Help("This identifier or symbol was not found in the current scope".into()))
                }),
                ExprKind::LetBinding {
                    name,
                    type_annotation,
                    value,
                } => {
                    let value_type = self.resolve_expr_type(value, type_annotation.as_ref())?;
                    self.ident_types.insert(name, value_type.clone());
                    Ok(value_type)
                }
                ExprKind::Literal(v) => self.check_literal(span, expected, v),
                ExprKind::Assign { target, value } => {
                    let target_type = self.resolve_expr_type(target, None)?;
                    let value_type = self.resolve_expr_type(value, None)?;
                    if target_type != value_type {
                        Err(crate::analysis_err!(
                            "Type mismatch: expected {:?}, got {:?}",
                            Some(span.to_display(self.interner)),
                            target_type,
                            value_type
                        )
                        .attach(Help("Ensure the types match or add an explicit conversion".into())))
                    } else {
                        Ok(target_type)
                    }
                }
                ExprKind::BinOp { left, right, op } => self.check_binary_op(left, right, op),
                ExprKind::Return { value } => {
                    if let Some(v) = value {
                        self.resolve_expr_type(v, expected)?;
                    }
                    Ok(VType::Primitive(PrimitiveTypes::Never))
                }
                ExprKind::Block { exprs } => {
                    if exprs.is_empty() {
                        Ok(VType::Primitive(PrimitiveTypes::Unit))
                    } else {
                        let mut last_expr_type = VType::Primitive(PrimitiveTypes::Unit);
                        for &expr_idx in &exprs {
                            match self.resolve_expr_type(expr_idx, None) {
                                Ok(ty) => last_expr_type = ty,
                                Err(e) => {
                                    self.diagnostics.push(e.into_cloneable());
                                }
                            }
                        }
                        Ok(last_expr_type)
                    }
                }
                ExprKind::If {
                    cond,
                    then_block,
                    else_block,
                } => {
                    let bool_type = VType::Primitive(PrimitiveTypes::Bool);
                    self.resolve_expr_type(cond, Some(&bool_type))?;
                    let then_type = self.resolve_expr_type(then_block, None)?;
                    if let Some(else_block) = else_block {
                        let else_type = self.resolve_expr_type(else_block, None)?;

                        if then_type == VType::Primitive(PrimitiveTypes::Never) && else_type == VType::Primitive(PrimitiveTypes::Never) {
                            Ok(VType::Primitive(PrimitiveTypes::Never))
                        } else if then_type == else_type {
                            Ok(then_type)
                        } else {
                            Err(crate::analysis_err!(
                                "If-Else branches do not match: expected {:?}, got {:?}",
                                Some(self.nodes.get(else_block).safe().span.to_display(self.interner)),
                                then_type,
                                else_type
                            )
                            .attach(Help("Ensure the branches match".into())))
                        }
                    } else {
                        Ok(VType::Primitive(PrimitiveTypes::Unit))
                    }
                }
                ExprKind::While { cond, body } => {
                    let bool_type = VType::Primitive(PrimitiveTypes::Bool);
                    self.resolve_expr_type(cond, Some(&bool_type))?;
                    self.resolve_expr_type(body, None)?;
                    Ok(VType::Primitive(PrimitiveTypes::Unit))
                }
                ExprKind::Break | ExprKind::Continue => Ok(VType::Primitive(PrimitiveTypes::Never)),
                ExprKind::Call { func, args } => {
                    let NodeKind::Expr {
                        kind: ExprKind::Identifier(func_name),
                        ..
                    } = &self.nodes.get(func).safe().kind
                    else {
                        return Err(crate::analysis_err!(
                            "Unsupported function call target: must be an identifier",
                            Some(span.to_display(self.interner))
                        )
                        .attach(Help("This feature is not yet implemented".into())));
                    };

                    let rest = self.fn_table.get(&func_name);
                    match rest {
                        Some(rest) => {
                            if args.len() != rest.0.len() {
                                return Err(crate::analysis_err!(
                                    "Type mismatch: expected {} argument(s), got {} argument(s)",
                                    Some(span.to_display(self.interner)),
                                    rest.0.len(),
                                    args.len()
                                )
                                .attach(Help("Ensure the types match or add an explicit conversion".into())));
                            }

                            let expected_types: Vec<_> = rest.0.iter().cloned().collect();
                            let return_type = rest.1.clone();

                            for (arg_idx, expected_type) in args.iter().zip(expected_types.iter()) {
                                let arg_type = self.resolve_expr_type(*arg_idx, Some(expected_type))?;
                                if arg_type != *expected_type {
                                    let span = self.nodes.get(*arg_idx).safe().span;
                                    return Err(crate::analysis_err!(
                                        "Type mismatch: expected {:?}, got {:?}",
                                        Some(span.to_display(self.interner)),
                                        expected_type,
                                        arg_type
                                    )
                                    .attach(Help("Ensure the types match or add an explicit conversion".into())));
                                }
                            }
                            Ok(return_type)
                        }
                        None => {
                            return Err(crate::analysis_err!(
                                "Unresolved function '{}'",
                                Some(span.to_display(self.interner)),
                                self.interner.resolve(*func_name).safe()
                            )
                            .attach(Help("This identifier or symbol was not found in the current scope".into())));
                        }
                    }
                }
                ExprKind::Cast { expr, target_type } => {
                    let expr_type = self.resolve_expr_type(expr, None)?;

                    if !can_convert(&expr_type, &target_type) {
                        return Err(crate::analysis_err!(
                            "Invalid cast: cannot cast type {:?} into {:?}",
                            Some(span.to_display(self.interner)),
                            expr_type,
                            target_type
                        )
                        .attach(Help("This construct is not valid in the current context".into())));
                    }

                    Ok(target_type)
                }
                ExprKind::UnaryOp { op, expr } => self.check_unary_op(expr, op),
                _ => {
                    return Err(
                        crate::analysis_err!("Unsupported expression kind: {:?}", Some(span.to_display(self.interner)), kind)
                            .attach(Help("This feature is not yet implemented".into())),
                    );
                }
            },
            _ => {
                return Err(crate::analysis_err!(
                    "Invalid node: expected expression node for type resolution",
                    Some(span.to_display(self.interner))
                )
                .attach(Help("This construct is not valid in the current context".into())));
            }
        }?;

        if let Some(expected_ty) = expected {
            if &inferred_type != expected_ty {
                return Err(crate::analysis_err!(
                    "Type mismatch: expected {:?}, got {:?}",
                    Some(span.to_display(self.interner)),
                    expected_ty,
                    inferred_type
                )
                .attach(Help("Ensure the types match or add an explicit conversion".into())));
            }
        }

        self.nodes.get_mut(target_idx).safe().set_type(inferred_type.clone());

        Ok(inferred_type)
    }
}
