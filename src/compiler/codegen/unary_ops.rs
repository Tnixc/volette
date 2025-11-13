use crate::{is_float, is_int};
use cranelift::prelude::FunctionBuilder;
use cranelift::prelude::{InstBuilder, Value};
use generational_arena::Index;

use crate::{
    SafeConvert,
    compiler::{
        codegen::{Info, Scopes, error::TranslateError, expr::expr_to_val},
        parser::node::{UnaryOpKind, VType},
    },
};

pub fn expr_unaryop(
    op: UnaryOpKind,
    expr: Index,
    fn_builder: &mut FunctionBuilder,
    scopes: &mut Scopes,
    info: &mut Info,
) -> Result<Value, TranslateError> {
    let (item, item_type) = expr_to_val(expr, fn_builder, scopes, info)?;
    if item.is_none() {};
    let item = item.safe();
    match (op, item_type) {
        (UnaryOpKind::Neg, VType::Primitive(is_int!())) => Ok(fn_builder.ins().ineg(item)),
        (UnaryOpKind::Neg, VType::Primitive(is_float!())) => Ok(fn_builder.ins().fneg(item)),
        (UnaryOpKind::Neg, _) => todo!(), // ERROR: wrong type
        _ => todo!(),                     // ERROR: unsupported op
    }
}
