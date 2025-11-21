use std::cmp::Ordering;

use cranelift::prelude::{FunctionBuilder, InstBuilder, Value, types};
use generational_arena::Index;
use rootcause::prelude::*;

use crate::{
    SafeConvert,
    compiler::{
        codegen::{Info, Scopes, ptr_width},
        error::Help,
        parser::node::VType,
        tokens::PrimitiveTypes,
    },
    is_float, is_int,
};

use super::expr::expr_to_val;

pub fn expr_cast(
    expr: Index,
    target_type: &VType,
    fn_builder: &mut FunctionBuilder,
    scopes: &mut Scopes,
    info: &mut Info,
) -> Result<Value, Report> {
    let (expr_val, expr_type) = expr_to_val(expr, fn_builder, scopes, info)?;
    let expr_val = expr_val.safe();

    let target_clif = target_type.to_clif(ptr_width());
    let source_clif = expr_type.to_clif(ptr_width());

    if source_clif == target_clif {
        return Ok(expr_val);
    }

    let result = match (&expr_type, target_type) {
        // int to int
        (VType::Primitive(from @ is_int!()), VType::Primitive(_to @ is_int!())) => match target_clif.bits().cmp(&source_clif.bits()) {
            Ordering::Less => fn_builder.ins().ireduce(target_clif, expr_val),
            Ordering::Greater => {
                if from.is_signed() {
                    fn_builder.ins().sextend(target_clif, expr_val)
                } else {
                    fn_builder.ins().uextend(target_clif, expr_val)
                }
            }
            Ordering::Equal => expr_val,
        },

        // float to float
        (VType::Primitive(is_float!()), VType::Primitive(is_float!())) => {
            if target_clif.bits() < source_clif.bits() {
                fn_builder.ins().fdemote(types::F32, expr_val)
            } else {
                fn_builder.ins().fpromote(types::F64, expr_val)
            }
        }

        // int to float
        (VType::Primitive(from @ is_int!()), VType::Primitive(is_float!())) => {
            if from.is_signed() {
                fn_builder.ins().fcvt_from_sint(target_clif, expr_val)
            } else {
                fn_builder.ins().fcvt_from_uint(target_clif, expr_val)
            }
        }

        // float to int
        (VType::Primitive(is_float!()), VType::Primitive(to @ is_int!())) => {
            if to.is_signed() {
                fn_builder.ins().fcvt_to_sint(target_clif, expr_val)
            } else {
                fn_builder.ins().fcvt_to_uint(target_clif, expr_val)
            }
        }

        // pointer to/from int conversions, nothing
        (VType::Pointer(_), VType::Primitive(PrimitiveTypes::Usize | PrimitiveTypes::Isize))
        | (VType::Primitive(PrimitiveTypes::Usize | PrimitiveTypes::Isize), VType::Pointer(_)) => expr_val,

        _ => {
            return Err(crate::codegen_err!(
                "Invalid cast: cannot cast from {:?} to {:?}",
                Some(info.nodes.get(expr).safe().span.to_display(info.interner)),
                expr_type,
                target_type
            )
            .attach(Help("This construct is not valid in the current context".into())));
        }
    };

    Ok(result)
}
