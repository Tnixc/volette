use cranelift::prelude::{FunctionBuilder, InstBuilder, Value, Variable};
use generational_arena::Index;
use std::collections::HashMap;
use string_interner::symbol::SymbolUsize;

use crate::compiler::{
    codegen::{Info, error::TranslateError},
    parser::node::{BinOpKind, Type},
    tokens::PrimitiveTypes,
};

use super::expr::expr_to_val;

pub fn expr_binop(
    left: Index,
    right: Index,
    op: BinOpKind,
    fn_builder: &mut FunctionBuilder,
    scopes: &mut Vec<HashMap<SymbolUsize, (Type, Variable)>>,
    info: &mut Info,
) -> Result<Value, TranslateError> {
    let (left_value, left_type) = expr_to_val(left, fn_builder, scopes, info)?;
    let (right_value, right_type) = expr_to_val(right, fn_builder, scopes, info)?;

    let val = match (left_type, right_type) {
        (Type::Primitive(PrimitiveTypes::F32), Type::Primitive(PrimitiveTypes::F32))
        | (Type::Primitive(PrimitiveTypes::F64), Type::Primitive(PrimitiveTypes::F64)) => {
            use cranelift::codegen::ir::condcodes::FloatCC;

            match op {
                BinOpKind::Add => fn_builder.ins().fadd(left_value, right_value),
                BinOpKind::Sub => fn_builder.ins().fsub(left_value, right_value),
                BinOpKind::Mul => fn_builder.ins().fmul(left_value, right_value),
                BinOpKind::Div => fn_builder.ins().fdiv(left_value, right_value),
                BinOpKind::Eq => fn_builder.ins().fcmp(FloatCC::Equal, left_value, right_value),
                BinOpKind::NotEq => fn_builder.ins().fcmp(FloatCC::NotEqual, left_value, right_value),
                BinOpKind::GreaterThan => fn_builder.ins().fcmp(FloatCC::GreaterThan, left_value, right_value),
                BinOpKind::GreaterThanOrEq => fn_builder.ins().fcmp(FloatCC::GreaterThanOrEqual, left_value, right_value),
                BinOpKind::LessThan => fn_builder.ins().fcmp(FloatCC::LessThan, left_value, right_value),
                BinOpKind::LessThanOrEq => fn_builder.ins().fcmp(FloatCC::LessThanOrEqual, left_value, right_value),
                _ => return Err(TranslateError::Internal("Unsupported float binary operation".to_string())),
            }
        }
        (Type::Primitive(PrimitiveTypes::I32), Type::Primitive(PrimitiveTypes::I32))
        | (Type::Primitive(PrimitiveTypes::I64), Type::Primitive(PrimitiveTypes::I64)) => {
            use cranelift::codegen::ir::condcodes::IntCC;
            match op {
                BinOpKind::Add => fn_builder.ins().iadd(left_value, right_value),
                BinOpKind::Sub => fn_builder.ins().isub(left_value, right_value),
                BinOpKind::Mul => fn_builder.ins().imul(left_value, right_value),
                BinOpKind::Div => fn_builder.ins().sdiv(left_value, right_value),
                BinOpKind::Eq => fn_builder.ins().icmp(IntCC::Equal, left_value, right_value),
                BinOpKind::NotEq => fn_builder.ins().icmp(IntCC::NotEqual, left_value, right_value),
                BinOpKind::GreaterThan => fn_builder.ins().icmp(IntCC::SignedGreaterThan, left_value, right_value),
                BinOpKind::GreaterThanOrEq => fn_builder.ins().icmp(IntCC::SignedGreaterThanOrEqual, left_value, right_value),
                BinOpKind::LessThan => fn_builder.ins().icmp(IntCC::SignedLessThan, left_value, right_value),
                BinOpKind::LessThanOrEq => fn_builder.ins().icmp(IntCC::SignedLessThanOrEqual, left_value, right_value),
                BinOpKind::Mod => fn_builder.ins().srem(left_value, right_value),
                _ => return Err(TranslateError::Internal("Unsupported integer binary operation".to_string())),
            }
        }

        _ => {
            return Err(TranslateError::Internal(
                "Unsupported type combination for binary operation".to_string(),
            ));
        }
    };

    Ok(val)
}
