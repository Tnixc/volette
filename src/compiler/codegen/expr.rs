use cranelift::prelude::{FunctionBuilder, InstBuilder, Value, Variable, types};
use generational_arena::Index;
use std::collections::HashMap;
use string_interner::symbol::SymbolUsize;

use crate::compiler::{
    codegen::{Info, error::TranslateError},
    parser::node::{BinOpKind, ExprKind, Literal, Node, NodeKind, Type},
    tokens::PrimitiveTypes,
};

pub fn expr_to_val(
    node: Index,
    fn_builder: &mut FunctionBuilder,
    scopes: &mut Vec<HashMap<SymbolUsize, Variable>>,
    info: &mut Info,
) -> Result<(Value, Type), TranslateError> {
    let node = info
        .nodes
        .get(node)
        .expect("[Expr to val] Node index not found in arena");
    match &node.kind {
        NodeKind::Expr { kind, type_ } => {
            if type_.is_none() {
                todo!("proper err here")
            }
            let value = match kind {
                ExprKind::Literal(literal) => match_literal(*literal, type_.unwrap(), node, fn_builder)?,
                ExprKind::BinOp { left, right, op } => expr_binop(*left, *right, *op, fn_builder, scopes, info)?,
                ExprKind::Return { value: ret_val } => expr_return(*ret_val, fn_builder, scopes, info)?,
                _ => todo!(),
            };

            Ok((value, type_.unwrap()))
        }
        _ => todo!(),
    }
}

fn expr_binop(
    left: Index,
    right: Index,
    op: BinOpKind,
    fn_builder: &mut FunctionBuilder,
    scopes: &mut Vec<HashMap<SymbolUsize, Variable>>,
    info: &mut Info,
) -> Result<Value, TranslateError> {
    let (left_value, left_type) = expr_to_val(left, fn_builder, scopes, info)?;
    let (right_value, right_type) = expr_to_val(right, fn_builder, scopes, info)?;
    let val = match (left_type, right_type) {
        (Type::Primitive(PrimitiveTypes::F32), Type::Primitive(PrimitiveTypes::F32))
        | (Type::Primitive(PrimitiveTypes::F64), Type::Primitive(PrimitiveTypes::F64)) => match op {
            BinOpKind::Add => fn_builder.ins().fadd(left_value, right_value),
            BinOpKind::Sub => fn_builder.ins().fsub(left_value, right_value),
            BinOpKind::Mul => fn_builder.ins().fmul(left_value, right_value),
            BinOpKind::Div => fn_builder.ins().fdiv(left_value, right_value),
            _ => todo!(),
        },
        (Type::Primitive(PrimitiveTypes::I32), Type::Primitive(PrimitiveTypes::I32))
        | (Type::Primitive(PrimitiveTypes::I64), Type::Primitive(PrimitiveTypes::I64)) => match op {
            BinOpKind::Add => fn_builder.ins().iadd(left_value, right_value),
            BinOpKind::Sub => fn_builder.ins().isub(left_value, right_value),
            BinOpKind::Mul => fn_builder.ins().imul(left_value, right_value),
            BinOpKind::Div => fn_builder.ins().sdiv(left_value, right_value),
            _ => todo!(),
        },

        _ => todo!(),
    };

    // match op {
    //     BinOpKind::Add => fn_builder.ins().iadd(left_value, right_value),
    //     BinOpKind::Sub => fn_builder.ins().isub(left_value, right_value),
    //     BinOpKind::Mul => fn_builder.ins().imul(left_value, right_value),
    // }
    Ok(val)
}

fn expr_return(
    ret_val: Option<Index>,
    fn_builder: &mut FunctionBuilder,
    scopes: &mut Vec<HashMap<SymbolUsize, Variable>>,
    info: &mut Info,
) -> Result<Value, TranslateError> {
    if let Some(ret_val) = ret_val {
        let (value, _) = expr_to_val(ret_val, fn_builder, scopes, info)?;
        fn_builder.ins().return_(&[value]);
        Ok(value)
    } else {
        fn_builder.ins().return_(&[]);
        Ok(Value::from_u32(0)) // TODO: implement proper never type
    }
}

fn match_literal(
    literal: Literal,
    type_: Type,
    node: &Node,
    fn_builder: &mut FunctionBuilder,
) -> Result<Value, TranslateError> {
    match literal {
        Literal::Bool(v) => {
            if type_ != Type::Primitive(PrimitiveTypes::Bool) {
                return Err(TranslateError::IncorrectTypeAnalysis {
                    type_,
                    node: node.clone(),
                });
            }
            if v {
                Ok(fn_builder.ins().iconst(types::I8, 1))
            } else {
                Ok(fn_builder.ins().iconst(types::I8, 0))
            }
        }
        Literal::Float(f) => match type_ {
            Type::Primitive(PrimitiveTypes::F64) => Ok(fn_builder.ins().f64const(f)),
            Type::Primitive(PrimitiveTypes::F32) => Ok(fn_builder.ins().f32const(f as f32)),
            _ => Err(TranslateError::IncorrectTypeAnalysis {
                type_,
                node: node.clone(),
            }),
        },
        Literal::Int(i) => match type_ {
            Type::Primitive(PrimitiveTypes::I64) | Type::Primitive(PrimitiveTypes::U64) => {
                Ok(fn_builder.ins().iconst(types::I64, i))
            }
            Type::Primitive(PrimitiveTypes::I32) | Type::Primitive(PrimitiveTypes::U32) => {
                Ok(fn_builder.ins().iconst(types::I32, i))
            }
            Type::Primitive(PrimitiveTypes::I16) | Type::Primitive(PrimitiveTypes::U16) => {
                Ok(fn_builder.ins().iconst(types::I16, i))
            }
            Type::Primitive(PrimitiveTypes::I8) | Type::Primitive(PrimitiveTypes::U8) => {
                Ok(fn_builder.ins().iconst(types::I8, i))
            }
            Type::Primitive(PrimitiveTypes::Isize) | Type::Primitive(PrimitiveTypes::Usize) => {
                Ok(fn_builder.ins().iconst(types::I64, i)) // Assuming 64-bit target
            }
            _ => Err(TranslateError::IncorrectTypeAnalysis {
                type_,
                node: node.clone(),
            }),
        },
        Literal::Nil => {
            if type_ != Type::Primitive(PrimitiveTypes::Nil) {
                return Err(TranslateError::IncorrectTypeAnalysis {
                    type_,
                    node: node.clone(),
                });
            }
            todo!("Nil literal not implemented");
        }
    }
}
