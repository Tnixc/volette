use cranelift::prelude::{FunctionBuilder, InstBuilder, Value};
use generational_arena::Index;
use rootcause::prelude::*;

use crate::{
    SafeConvert,
    compiler::{
        codegen::{Info, Scopes},
        error::Help,
        parser::node::{BinOpKind, VType},
    },
    is_float, is_int,
};

use super::expr::expr_to_val;

pub fn expr_binop(
    left: Index,
    right: Index,
    op: BinOpKind,
    fn_builder: &mut FunctionBuilder,
    scopes: &mut Scopes,
    info: &mut Info,
) -> Result<Value, Report> {
    let (left_value, left_type) = expr_to_val(left, fn_builder, scopes, info)?;
    let left_value = left_value.safe(); // type checker should catch bin ops with Nil/zero sized types?
    let (right_value, right_type) = expr_to_val(right, fn_builder, scopes, info)?;
    let right_value = right_value.safe();

    let val = match (&left_type.coerced(), &right_type.coerced()) {
        (VType::Primitive(pt @ is_float!()), VType::Primitive(rt)) if pt == rt => {
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
                _ => {
                    return Err(crate::codegen_err!(
                        "Unsupported binary operation '{:?}' on floats",
                        Some(info.nodes.get(left).safe().span.to_display(info.interner)),
                        op
                    )
                    .attach(Help("This operation is not supported for floating-point types".into())));
                }
            }
        }
        (VType::Primitive(pt @ is_int!()), VType::Primitive(rt)) if pt == rt => {
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
                BinOpKind::BitwiseAnd => fn_builder.ins().band(left_value, right_value),
                BinOpKind::BitwiseOr => fn_builder.ins().bor(left_value, right_value),
                BinOpKind::BitwiseXor => fn_builder.ins().bxor(left_value, right_value),
                BinOpKind::BitwiseShLeft => fn_builder.ins().ishl(left_value, right_value),
                BinOpKind::BitwiseShRight => fn_builder.ins().sshr(left_value, right_value),
                _ => {
                    return Err(crate::codegen_err!(
                        "Unsupported binary operation '{:?}' on integers",
                        Some(info.nodes.get(left).safe().span.to_display(info.interner)),
                        op
                    )
                    .attach(Help("This operation is not supported for integer types".into())));
                }
            }
        }

        _ => {
            eprintln!("TYPE LEFT, {:?}, OPERAND: {:?}, TYPE RIGHT, {:?}", left_type, op, right_type);
            return Err(crate::codegen_err!(
                "Unsupported binary operation '{:?}' with types {:?} and {:?}",
                Some(info.nodes.get(left).safe().span.to_display(info.interner)),
                op,
                left_type,
                right_type
            )
            .attach(Help("This type combination is not supported".into())));
        }
    };

    Ok(val)
}
