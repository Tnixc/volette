use std::collections::HashMap;

use generational_arena::{Arena, Index};
use rootcause::prelude::*;
use string_interner::{StringInterner, backend::BucketBackend, symbol::SymbolUsize};

use crate::{
    SafeConvert,
    compiler::{
        analysis::{binop::check_binop, compatible::can_convert, literal::check_literal, unaryop::check_unaryop},
        error::{Help, ReportCollection},
        parser::node::{DefKind, ExprKind, Node, NodeKind, VType},
        tokens::PrimitiveTypes,
    },
};

pub fn type_check_root(
    root: &Node,
    interner: &StringInterner<BucketBackend<SymbolUsize>>,
    nodes: &mut Arena<Node>,
    fn_table: &HashMap<SymbolUsize, (Box<Vec<VType>>, VType)>,
) -> Result<(), ReportCollection> {
    let mut diagnostics = ReportCollection::new();

    if let NodeKind::Root { defs } = &root.kind {
        for idx in defs {
            let node = nodes.get(*idx).safe();
            if let NodeKind::Def { kind } = &node.kind {
                if let DefKind::Function {
                    body, params, return_type, ..
                } = kind
                {
                    let body_idx = *body;
                    let return_type = return_type.clone();

                    let mut ident_types: HashMap<SymbolUsize, VType> = HashMap::new();
                    params.iter().for_each(|param| {
                        ident_types.insert(param.0, param.1.clone());
                    });

                    // type check the function body
                    // the body is a block expression that should ultimately return the declared return_type
                    let error_count_before = diagnostics.len();
                    if let Err(e) = resolve_expr_type(body_idx, None, nodes, interner, &mut ident_types, &fn_table, &mut diagnostics) {
                        diagnostics.push(e.into_cloneable());
                    }
                    let had_body_errors = diagnostics.len() > error_count_before;

                    // a body with type never is compatible with any return type
                    // only check return type if there were no errors in the body (to avoid cascading errors)
                    if !had_body_errors {
                        let body_node = nodes.get(body_idx).safe();
                        if let NodeKind::Expr {
                            type_: Some(body_type), ..
                        } = &body_node.kind
                        {
                            if *body_type != VType::Primitive(PrimitiveTypes::Never) && *body_type != return_type {
                                diagnostics.push(
                                    crate::analysis_err!(
                                        "Type mismatch: expected {:?}, got {:?}",
                                        Some(body_node.span.to_display(interner)),
                                        return_type,
                                        body_type
                                    )
                                    .attach(Help("Ensure the types match or add an explicit conversion".into()))
                                    .into_cloneable(),
                                );
                            }
                        }
                    }
                }
            }
        }
    }

    if !diagnostics.is_empty() { Err(diagnostics) } else { Ok(()) }
}

/// resolves the type of an expression.
/// if `expected` is `some`, it checks the expression against that type.
/// if `expected` is `none`, it infers the expression's type.
pub(crate) fn resolve_expr_type(
    target_idx: Index,
    expected: Option<&VType>,
    nodes: &mut Arena<Node>,
    interner: &StringInterner<BucketBackend<SymbolUsize>>,
    ident_types: &mut HashMap<SymbolUsize, VType>,
    fn_table: &HashMap<SymbolUsize, (Box<Vec<VType>>, VType)>,
    diagnostics: &mut ReportCollection,
) -> Result<VType, Report> {
    let (kind, span) = {
        let target_node = nodes.get(target_idx).safe();
        // if type is already resolved, return it and check if it matches expectations
        if let NodeKind::Expr { type_: Some(ty), .. } = &target_node.kind {
            if let Some(expected_ty) = expected
                && ty != expected_ty
            {
                return Err(crate::analysis_err!(
                    "Type mismatch: expected {:?}, got {:?}",
                    Some(target_node.span.to_display(interner)),
                    expected_ty,
                    ty
                )
                .attach(Help("Ensure the types match or add an explicit conversion".into())));
            }
            return Ok(ty.clone());
        }
        (target_node.kind.clone(), target_node.span)
    };

    // NOTE: here is resolve expr type
    //
    let inferred_type = match kind {
        NodeKind::Expr { kind, .. } => match kind {
            ExprKind::Identifier(symbol) => ident_types.get(&symbol).cloned().ok_or_else(|| {
                crate::analysis_err!(
                    "Unresolved identifier '{}'",
                    Some(span.to_display(interner)),
                    interner.resolve(symbol).unwrap_or("<unknown>")
                )
                .attach(Help("This identifier or symbol was not found in the current scope".into()))
            }),
            ExprKind::LetBinding {
                name,
                type_annotation,
                value,
            } => {
                let value_type = resolve_expr_type(value, type_annotation.as_ref(), nodes, interner, ident_types, fn_table, diagnostics)?;
                // println!("Value type: {}", value_type);
                ident_types.insert(name, value_type.clone());
                Ok(value_type)
            }
            ExprKind::Literal(v) => check_literal(span, interner, expected, v),
            ExprKind::Assign { target, value } => {
                let target_type = resolve_expr_type(target, None, nodes, interner, ident_types, fn_table, diagnostics)?;
                let value_type = resolve_expr_type(value, None, nodes, interner, ident_types, fn_table, diagnostics)?;
                if target_type != value_type {
                    Err(crate::analysis_err!(
                        "Type mismatch: expected {:?}, got {:?}",
                        Some(span.to_display(interner)),
                        target_type,
                        value_type
                    )
                    .attach(Help("Ensure the types match or add an explicit conversion".into())))
                } else {
                    Ok(target_type)
                }
            }
            ExprKind::BinOp { left, right, op } => check_binop(left, right, op, nodes, interner, ident_types, fn_table, diagnostics),
            ExprKind::Return { value } => {
                if let Some(v) = value {
                    resolve_expr_type(v, expected, nodes, interner, ident_types, fn_table, diagnostics)?;
                }
                Ok(VType::Primitive(PrimitiveTypes::Never))
            }
            ExprKind::Block { exprs } => {
                if exprs.is_empty() {
                    Ok(VType::Primitive(PrimitiveTypes::Nil))
                } else {
                    let mut last_expr_type = VType::Primitive(PrimitiveTypes::Nil);
                    for &expr_idx in &exprs {
                        match resolve_expr_type(expr_idx, None, nodes, interner, ident_types, fn_table, diagnostics) {
                            Ok(ty) => last_expr_type = ty,
                            Err(e) => {
                                diagnostics.push(e.into_cloneable());
                                // continues
                            }
                        }
                    }

                    // the block type is the type of its last expression (??)
                    Ok(last_expr_type)
                }
            }
            ExprKind::If {
                cond,
                then_block,
                else_block,
            } => {
                let bool_type = VType::Primitive(PrimitiveTypes::Bool);
                resolve_expr_type(cond, Some(&bool_type), nodes, interner, ident_types, fn_table, diagnostics)?;
                let then_type = resolve_expr_type(then_block, None, nodes, interner, ident_types, fn_table, diagnostics)?;
                if let Some(else_block) = else_block {
                    let else_type = resolve_expr_type(else_block, None, nodes, interner, ident_types, fn_table, diagnostics)?;

                    // if both branches diverge (never), the if-expression diverges
                    if then_type == VType::Primitive(PrimitiveTypes::Never) && else_type == VType::Primitive(PrimitiveTypes::Never) {
                        Ok(VType::Primitive(PrimitiveTypes::Never))
                    } else if then_type == else_type {
                        Ok(then_type)
                    } else {
                        Err(crate::analysis_err!(
                            "If-Else branches do not match: expected {:?}, got {:?}",
                            Some(nodes.get(else_block).safe().span.to_display(interner)),
                            then_type,
                            else_type
                        )
                        .attach(Help("Ensure the branches match".into())))
                    }
                } else {
                    // if without else should return nil when condition is false
                    Ok(VType::Primitive(PrimitiveTypes::Nil))
                }
            }
            ExprKind::While { cond, .. } => {
                let bool_type = VType::Primitive(PrimitiveTypes::Bool);
                resolve_expr_type(cond, Some(&bool_type), nodes, interner, ident_types, fn_table, diagnostics)?;
                Ok(VType::Primitive(PrimitiveTypes::Nil))
            }
            ExprKind::Call { func, args } => {
                let NodeKind::Expr {
                    kind: ExprKind::Identifier(func_name),
                    ..
                } = &nodes.get(func).safe().kind
                else {
                    return Err(crate::analysis_err!(
                        "Unsupported function call target: must be an identifier",
                        Some(span.to_display(interner))
                    )
                    .attach(Help("This feature is not yet implemented".into())));
                };

                let rest = fn_table.get(&func_name);
                match rest {
                    Some(rest) => {
                        // check arity
                        if args.len() != rest.0.len() {
                            return Err(crate::analysis_err!(
                                "Type mismatch: expected {} argument(s), got {} argument(s)",
                                Some(span.to_display(interner)),
                                rest.0.len(),
                                args.len()
                            )
                            .attach(Help("Ensure the types match or add an explicit conversion".into())));
                        }

                        // check argument types
                        for (arg_idx, expected_type) in args.iter().zip(rest.0.iter()) {
                            let arg_type =
                                resolve_expr_type(*arg_idx, Some(expected_type), nodes, interner, ident_types, fn_table, diagnostics)?;
                            if arg_type != *expected_type {
                                let span = nodes.get(*arg_idx).safe().span;
                                return Err(crate::analysis_err!(
                                    "Type mismatch: expected {:?}, got {:?}",
                                    Some(span.to_display(interner)),
                                    expected_type,
                                    arg_type
                                )
                                .attach(Help("Ensure the types match or add an explicit conversion".into())));
                            }
                        }
                        Ok(rest.1.clone())
                    }
                    None => {
                        return Err(crate::analysis_err!(
                            "Unresolved function '{}'",
                            Some(span.to_display(interner)),
                            interner.resolve(*func_name).safe()
                        )
                        .attach(Help("This identifier or symbol was not found in the current scope".into())));
                    }
                }
            }
            ExprKind::Cast { expr, target_type } => {
                let expr_type = resolve_expr_type(expr, None, nodes, interner, ident_types, fn_table, diagnostics)?;

                if !can_convert(&expr_type, &target_type) {
                    return Err(crate::analysis_err!(
                        "Invalid cast: cannot cast type {:?} into {:?}",
                        Some(span.to_display(interner)),
                        expr_type,
                        target_type
                    )
                    .attach(Help("This construct is not valid in the current context".into())));
                }

                Ok(target_type)
            }
            ExprKind::UnaryOp { op, expr } => check_unaryop(expr, op, nodes, interner, ident_types, fn_table, diagnostics),
            _ => {
                return Err(
                    crate::analysis_err!("Unsupported expression kind: {:?}", Some(span.to_display(interner)), kind)
                        .attach(Help("This feature is not yet implemented".into())),
                );
            }
        },
        _ => {
            return Err(crate::analysis_err!(
                "Invalid node: expected expression node for type resolution",
                Some(span.to_display(interner))
            )
            .attach(Help("This construct is not valid in the current context".into())));
        }
    }?;

    // if an expected type was provided, check if the inferred type matches
    // TODO: probably needs to check for coercion as well
    if let Some(expected_ty) = expected {
        if &inferred_type != expected_ty {
            return Err(crate::analysis_err!(
                "Type mismatch: expected {:?}, got {:?}",
                Some(span.to_display(interner)),
                expected_ty,
                inferred_type
            )
            .attach(Help("Ensure the types match or add an explicit conversion".into())));
        }
    }

    // update the node with the resolved type
    nodes.get_mut(target_idx).safe().set_type(inferred_type.clone());

    Ok(inferred_type)
}
