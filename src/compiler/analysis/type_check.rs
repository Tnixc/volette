use std::collections::HashMap;

use generational_arena::{Arena, Index};
use string_interner::{backend::BucketBackend, symbol::SymbolUsize, StringInterner};

use crate::compiler::{
    parser::node::{DefKind, ExprKind, Literal, Node, NodeKind, Type},
    tokens::PrimitiveTypes,
};

use super::error::AnalysisError;

pub fn type_check_root(root: &Node, interner: StringInterner<BucketBackend<SymbolUsize>>, nodes: Arena<Node>) {
    let ident_types: HashMap<SymbolUsize, Type> = HashMap::new();
    match &root.kind {
        NodeKind::Root { defs } => {
            for idx in defs {
                let node = nodes.get(*idx).expect("[!] Node not found");
                match &node.kind {
                    NodeKind::Def { kind } => match kind {
                        DefKind::Function { return_type, body, .. } => {}
                        _ => unimplemented!(),
                    },
                    _ => unreachable!(),
                }
            }
        }
        _ => unreachable!(),
    }
}

fn resolve_expr_type(
    target: &Node,
    interner: StringInterner<BucketBackend<SymbolUsize>>,
    nodes: Arena<Node>,
    expected: Type,
    ident_types: &mut HashMap<SymbolUsize, Type>,
) -> Result<Type, AnalysisError> {
    match &target.kind {
        NodeKind::Expr { kind, type_ } => {
            if let Some(t) = type_ {
                return Ok(*t);
            }

            return match kind {
                ExprKind::Identifier(symbol) => {
                    if let Some(t) = ident_types.get(symbol) {
                        Ok(*t)
                    } else {
                        let name = interner.resolve(*symbol).expect("never").to_string();
                        Err(AnalysisError::UnresolvedIdentifier {
                            name,
                            span: target.span.to_display(&interner),
                        })
                    }
                }
                ExprKind::LetBinding {
                    name,
                    type_annotation,
                    value,
                } => resolve_let_binding(name, interner, nodes, *type_annotation, value, ident_types),
                ExprKind::Literal(v) => resolve_literal(target, interner, expected, *v),
                ExprKind::Return { .. } => Ok(Type::Primitive(PrimitiveTypes::Never)),
                _ => todo!(),
            };
        }
        _ => unreachable!(),
    }
}

fn resolve_let_binding(
    name: &SymbolUsize,
    interner: StringInterner<BucketBackend<SymbolUsize>>,
    nodes: Arena<Node>,
    type_annotation: Option<Type>,
    value: &Index,
    ident_types: &mut HashMap<SymbolUsize, Type>,
) -> Result<Type, AnalysisError> {
    let value = &nodes.get(*value).expect("Node not found").clone();
    let expected = type_annotation.expect("Declarations without type hints are not supported");
    let value_type = resolve_expr_type(value, interner, nodes, expected, ident_types)?;

    ident_types.insert(*name, value_type);
    Ok(value_type)
}

fn resolve_literal(
    target: &Node,
    interner: StringInterner<BucketBackend<SymbolUsize>>,
    expected: Type,
    literal: Literal,
) -> Result<Type, AnalysisError> {
    match literal {
        Literal::Bool(_) => match expected {
            Type::Primitive(PrimitiveTypes::Bool) => Ok(Type::Primitive(PrimitiveTypes::Bool)),
            _ => Err(AnalysisError::TypeMismatch {
                expected,
                got: Type::Primitive(PrimitiveTypes::Bool),
                span: target.span.to_display(&interner),
            }),
        },
        Literal::Int(_) => match expected {
            Type::Primitive(PrimitiveTypes::U8) => Ok(Type::Primitive(PrimitiveTypes::U8)),
            Type::Primitive(PrimitiveTypes::U16) => Ok(Type::Primitive(PrimitiveTypes::U16)),
            Type::Primitive(PrimitiveTypes::U32) => Ok(Type::Primitive(PrimitiveTypes::U32)),
            Type::Primitive(PrimitiveTypes::U64) => Ok(Type::Primitive(PrimitiveTypes::U64)),
            Type::Primitive(PrimitiveTypes::I8) => Ok(Type::Primitive(PrimitiveTypes::I8)),
            Type::Primitive(PrimitiveTypes::I16) => Ok(Type::Primitive(PrimitiveTypes::I16)),
            Type::Primitive(PrimitiveTypes::I32) => Ok(Type::Primitive(PrimitiveTypes::I32)),
            Type::Primitive(PrimitiveTypes::I64) => Ok(Type::Primitive(PrimitiveTypes::I64)),
            _ => Err(AnalysisError::TypeMismatch {
                expected,
                got: Type::Primitive(PrimitiveTypes::U8),
                span: target.span.to_display(&interner),
            }),
        },
        Literal::Float(_) => match expected {
            Type::Primitive(PrimitiveTypes::F32) => Ok(Type::Primitive(PrimitiveTypes::F32)),
            Type::Primitive(PrimitiveTypes::F64) => Ok(Type::Primitive(PrimitiveTypes::F64)),
            _ => Err(AnalysisError::TypeMismatch {
                expected,
                got: Type::Primitive(PrimitiveTypes::F32),
                span: target.span.to_display(&interner),
            }),
        },
        _ => todo!(), // TODO: What do I even do for nil
                      // resolve expected via idnet map first
    }
}
