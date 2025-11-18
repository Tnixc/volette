use std::collections::HashMap;

use generational_arena::{Arena, Index};
use string_interner::{StringInterner, backend::BucketBackend, symbol::SymbolUsize};

use crate::{
    SafeConvert,
    compiler::{
        analysis::{binop::check_binop, compatible::can_convert, unaryop::check_unaryop},
        error::DiagnosticCollection,
        parser::node::{DefKind, ExprKind, Literal, Node, NodeKind, VType},
        tokens::{PrimitiveTypes, Span},
    },
    is_float, is_int,
};

use super::error::AnalysisError;

pub fn type_check_root(
    root: &Node,
    interner: &StringInterner<BucketBackend<SymbolUsize>>,
    nodes: &mut Arena<Node>,
    fn_table: &HashMap<SymbolUsize, (Box<Vec<VType>>, VType)>,
) -> Result<(), DiagnosticCollection> {
    let mut diagnostics = DiagnosticCollection::new();

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
                    let error_count_before = diagnostics.diagnostics.len();
                    if let Err(e) = resolve_expr_type(body_idx, None, nodes, interner, &mut ident_types, &fn_table, &mut diagnostics) {
                        diagnostics.add_error(e);
                    }
                    let had_body_errors = diagnostics.diagnostics.len() > error_count_before;

                    // a body with type never is compatible with any return type
                    // only check return type if there were no errors in the body (to avoid cascading errors)
                    if !had_body_errors {
                        let body_node = nodes.get(body_idx).safe();
                        if let NodeKind::Expr {
                            type_: Some(body_type), ..
                        } = &body_node.kind
                        {
                            if *body_type != VType::Primitive(PrimitiveTypes::Never) && *body_type != return_type {
                                diagnostics.add_error(AnalysisError::TypeMismatch {
                                    expected: format!("{:?}", return_type),
                                    got: format!("{:?}", body_type),
                                    span: body_node.span.to_display(interner),
                                });
                            }
                        }
                    }
                }
            }
        }
    }

    if diagnostics.has_errors() { Err(diagnostics) } else { Ok(()) }
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
    diagnostics: &mut DiagnosticCollection,
) -> Result<VType, AnalysisError> {
    let (kind, span) = {
        let target_node = nodes.get(target_idx).safe();
        // if type is already resolved, return it and check if it matches expectations
        if let NodeKind::Expr { type_: Some(ty), .. } = &target_node.kind {
            if let Some(expected_ty) = expected
                && ty != expected_ty
            {
                return Err(AnalysisError::TypeMismatch {
                    expected: format!("{:?}", expected_ty),
                    got: format!("{:?}", ty),
                    span: target_node.span.to_display(interner),
                });
            }
            return Ok(ty.clone());
        }
        (target_node.kind.clone(), target_node.span)
    };

    let inferred_type = match kind {
        NodeKind::Expr { kind, .. } => match kind {
            ExprKind::Identifier(symbol) => ident_types.get(&symbol).cloned().ok_or_else(|| AnalysisError::Unresolved {
                what: format!("identifier '{}'", interner.resolve(symbol).unwrap_or("<unknown>")),
                span: span.to_display(interner),
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
                    Err(AnalysisError::TypeMismatch {
                        expected: format!("{:?}", target_type),
                        got: format!("{:?}", value_type),
                        span: span.to_display(interner),
                    })
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
                                diagnostics.add_error(e);
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
                        Err(AnalysisError::TypeMismatch {
                            expected: format!("{:?}", then_type),
                            got: format!("{:?}", else_type),
                            span: nodes.get(else_block).safe().span.to_display(interner),
                        })
                    }
                } else {
                    // if without else should return nil when condition is false
                    Ok(VType::Primitive(PrimitiveTypes::Nil))
                }
            }
            ExprKind::Call { func, args } => {
                let NodeKind::Expr {
                    kind: ExprKind::Identifier(func_name),
                    ..
                } = &nodes.get(func).safe().kind
                else {
                    return Err(AnalysisError::Unsupported {
                        what: "function call target must be an identifier".to_string(),
                        span: span.to_display(interner),
                    });
                };

                let rest = fn_table.get(&func_name);
                match rest {
                    Some(rest) => {
                        // check arity
                        if args.len() != rest.0.len() {
                            return Err(AnalysisError::TypeMismatch {
                                expected: format!("{} argument(s)", rest.0.len()),
                                got: format!("{} argument(s)", args.len()),
                                span: span.to_display(interner),
                            });
                        }

                        // check argument types
                        for (arg_idx, expected_type) in args.iter().zip(rest.0.iter()) {
                            let arg_type =
                                resolve_expr_type(*arg_idx, Some(expected_type), nodes, interner, ident_types, fn_table, diagnostics)?;
                            if arg_type != *expected_type {
                                let span = nodes.get(*arg_idx).safe().span;
                                return Err(AnalysisError::TypeMismatch {
                                    expected: format!("{:?}", expected_type),
                                    got: format!("{:?}", arg_type),
                                    span: span.to_display(interner),
                                });
                            }
                        }
                        Ok(rest.1.clone())
                    }
                    None => {
                        return Err(AnalysisError::Unresolved {
                            what: format!("function '{}'", interner.resolve(*func_name).safe()),
                            span: span.to_display(interner),
                        });
                    }
                }
            }
            ExprKind::Cast { expr, target_type } => {
                let expr_type = resolve_expr_type(expr, None, nodes, interner, ident_types, fn_table, diagnostics)?;

                if !can_convert(&expr_type, &target_type) {
                    return Err(AnalysisError::Invalid {
                        what: "cast".to_string(),
                        reason: format!("cannot cast type {:?} into {:?}", expr_type, target_type),
                        span: span.to_display(interner),
                    });
                }

                Ok(target_type)
            }
            ExprKind::UnaryOp { op, expr } => check_unaryop(expr, op, nodes, interner, ident_types, fn_table, diagnostics),
            _ => {
                return Err(AnalysisError::Unsupported {
                    what: format!("expression kind: {:?}", kind),
                    span: span.to_display(interner),
                });
            }
        },
        _ => {
            return Err(AnalysisError::Invalid {
                what: "node".to_string(),
                reason: "expected expression node for type resolution".to_string(),
                span: span.to_display(interner),
            });
        }
    }?;

    // if an expected type was provided, check if the inferred type matches
    // TODO: probably needs to check for coercion as well
    if let Some(expected_ty) = expected {
        if &inferred_type != expected_ty {
            return Err(AnalysisError::TypeMismatch {
                expected: format!("{:?}", expected_ty),
                got: format!("{:?}", inferred_type),
                span: span.to_display(interner),
            });
        }
    }

    // update the node with the resolved type
    nodes.get_mut(target_idx).safe().set_type(inferred_type.clone());

    Ok(inferred_type)
}

fn check_literal(
    span: Span,
    interner: &StringInterner<BucketBackend<SymbolUsize>>,
    expected: Option<&VType>,
    literal: Literal,
) -> Result<VType, AnalysisError> {
    let inferred = match literal {
        Literal::Int(_) => VType::Primitive(PrimitiveTypes::I32),
        Literal::Float(_) => VType::Primitive(PrimitiveTypes::F32),
        Literal::Bool(_) => VType::Primitive(PrimitiveTypes::Bool),
        Literal::Nil => VType::Primitive(PrimitiveTypes::Nil),
    };

    let target = expected.unwrap_or(&inferred);

    // can coerce?
    match (literal, target) {
        (Literal::Int(_), VType::Primitive(is_int!())) => Ok(target.clone()),
        (Literal::Float(_), VType::Primitive(is_float!())) => Ok(target.clone()),
        (Literal::Bool(_), VType::Primitive(PrimitiveTypes::Bool)) => Ok(target.clone()),
        (Literal::Nil, VType::Primitive(PrimitiveTypes::Nil)) => Ok(target.clone()),
        _ => Err(AnalysisError::TypeMismatch {
            expected: format!("{:?}", target),
            got: format!("{:?}", inferred),
            span: span.to_display(interner),
        }),
    }
}
