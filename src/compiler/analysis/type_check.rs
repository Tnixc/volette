use std::collections::HashMap;

use generational_arena::{Arena, Index};
use string_interner::{StringInterner, backend::BucketBackend, symbol::SymbolUsize};

use crate::compiler::{
    parser::node::{BinOpKind, DefKind, ExprKind, Literal, Node, NodeKind, Type},
    tokens::PrimitiveTypes,
};

use super::error::AnalysisError;

pub fn type_check_root(root: &Node, interner: &StringInterner<BucketBackend<SymbolUsize>>, nodes: &mut Arena<Node>) {
    if let NodeKind::Root { defs } = &root.kind {
        for idx in defs {
            let node = nodes.get(*idx).expect("[!] Node not found");
            if let NodeKind::Def { kind } = &node.kind {
                // TODO: check return types
                if let DefKind::Function { body, params, .. } = kind {
                    let mut ident_types: HashMap<SymbolUsize, Type> = HashMap::new();
                    params.iter().for_each(|param| {
                        ident_types.insert(param.0, param.1);
                    });
                    let body_node = nodes.get(*body).expect("Node not found").clone();
                    if let NodeKind::Expr { kind, .. } = &body_node.kind {
                        if let ExprKind::Block { exprs } = kind {
                            for &expr_idx in exprs {
                                if let Err(e) = resolve_expr_type(expr_idx, None, nodes, interner, &mut ident_types) {
                                    eprintln!("Type Error: {:?}", e);
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

/// resolves the type of an expression.
/// if `expected` is `some`, it checks the expression against that type.
/// if `expected` is `none`, it infers the expression's type.
fn resolve_expr_type(
    target_idx: Index,
    expected: Option<Type>,
    nodes: &mut Arena<Node>,
    interner: &StringInterner<BucketBackend<SymbolUsize>>,
    ident_types: &mut HashMap<SymbolUsize, Type>,
) -> Result<Type, AnalysisError> {
    let (kind, span) = {
        let target_node = nodes.get(target_idx).expect("node not found");
        // if type is already resolved, return it and check if it matches expectations
        if let NodeKind::Expr { type_: Some(ty), .. } = target_node.kind {
            if let Some(expected_ty) = expected {
                if ty != expected_ty {
                    return Err(AnalysisError::TypeMismatch {
                        expected: expected_ty,
                        got: ty,
                        span: target_node.span.to_display(interner),
                    });
                }
            }
            return Ok(ty);
        }
        (target_node.kind.clone(), target_node.span)
    };

    let inferred_type = match kind {
        NodeKind::Expr { kind, .. } => match kind {
            ExprKind::Identifier(symbol) => {
                ident_types
                    .get(&symbol)
                    .copied()
                    .ok_or_else(|| AnalysisError::UnresolvedIdentifier {
                        name: interner.resolve(symbol).unwrap_or("").to_string(),
                        span: span.to_display(interner),
                    })
            }
            ExprKind::LetBinding {
                name,
                type_annotation,
                value,
            } => {
                let value_type = resolve_expr_type(value, type_annotation, nodes, interner, ident_types)?;
                println!("Value type: {}", value_type);
                ident_types.insert(name, value_type);
                Ok(value_type)
            }
            ExprKind::Literal(v) => resolve_literal(span, interner, expected, v),
            ExprKind::BinOp { left, right, op } => resolve_binop(left, right, op, nodes, interner, ident_types),
            ExprKind::Return { .. } => Ok(Type::Primitive(PrimitiveTypes::Never)),
            _ => todo!(),
        },
        _ => unreachable!(),
    }?;

    // if an expected type was provided, check if the inferred type matches
    // TODO: probably needs to check for coercion as well
    if let Some(expected_ty) = expected {
        if inferred_type != expected_ty {
            return Err(AnalysisError::TypeMismatch {
                expected: expected_ty,
                got: inferred_type,
                span: span.to_display(interner),
            });
        }
    }

    // update the node with the resolved type
    nodes
        .get_mut(target_idx)
        .expect("Node not found")
        .set_type(inferred_type);

    Ok(inferred_type)
}

fn resolve_literal(
    span: crate::compiler::tokens::Span,
    interner: &StringInterner<BucketBackend<SymbolUsize>>,
    expected: Option<Type>,
    literal: Literal,
) -> Result<Type, AnalysisError> {
    let literal_type = match literal {
        Literal::Int(_) => expected.unwrap_or(Type::Primitive(PrimitiveTypes::I32)),
        Literal::Float(_) => expected.unwrap_or(Type::Primitive(PrimitiveTypes::F32)),
        Literal::Bool(_) => Type::Primitive(PrimitiveTypes::Bool),
        Literal::Nil => Type::Primitive(PrimitiveTypes::Nil),
    };

    // check if the literal can be coerced to the expected type
    match (literal, literal_type) {
        (Literal::Int(_), Type::Primitive(pt)) if pt.is_integer() => Ok(literal_type),
        (Literal::Float(_), Type::Primitive(pt)) if pt.is_float() => Ok(literal_type),
        (Literal::Bool(_), Type::Primitive(PrimitiveTypes::Bool)) => Ok(literal_type),
        (Literal::Nil, Type::Primitive(PrimitiveTypes::Nil)) => Ok(literal_type),
        _ => Err(AnalysisError::TypeMismatch {
            expected: expected.unwrap_or(literal_type),
            got: literal_type,
            span: span.to_display(interner),
        }),
    }
}

fn resolve_binop(
    left: Index,
    right: Index,
    op: BinOpKind,
    nodes: &mut Arena<Node>,
    interner: &StringInterner<BucketBackend<SymbolUsize>>,
    ident_types: &mut HashMap<SymbolUsize, Type>,
) -> Result<Type, AnalysisError> {
    let left_ty = resolve_expr_type(left, None, nodes, interner, ident_types)?;
    let right_ty = resolve_expr_type(right, Some(left_ty), nodes, interner, ident_types)?;

    if left_ty != right_ty {
        return Err(AnalysisError::TypeMismatch {
            expected: left_ty,
            got: right_ty,
            span: nodes.get(right).unwrap().span.to_display(interner),
        });
    }

    match op {
        BinOpKind::Add | BinOpKind::Sub | BinOpKind::Mul | BinOpKind::Div | BinOpKind::Mod => {
            // TODO??
            Ok(left_ty)
        }
        BinOpKind::Eq
        | BinOpKind::NotEq
        | BinOpKind::LessThan
        | BinOpKind::GreaterThanOrEq
        | BinOpKind::GreaterThan
        | BinOpKind::LessThanOrEq => Ok(Type::Primitive(PrimitiveTypes::Bool)),
        _ => Err(AnalysisError::Internal("Unsupported binary operator.".to_string())),
    }
}
