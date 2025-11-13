use crate::compiler::codegen::PtrWidth;
use crate::compiler::tokens::PrimitiveTypes;
use crate::{is_float, is_int};
use cranelift::prelude::{FunctionBuilder, IntCC, StackSlotData, StackSlotKind};
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
        (UnaryOpKind::Not, VType::Primitive(PrimitiveTypes::Bool)) => Ok(fn_builder.ins().icmp_imm(IntCC::Equal, item, 0)),
        (UnaryOpKind::AddressOf, VType::Primitive(ty)) => {
            let size = ty.to_clif(info.build_config.ptr_width).bytes();
            let stack_slot = fn_builder.create_sized_stack_slot(StackSlotData::new(StackSlotKind::ExplicitSlot, size, 0));
            let addr = fn_builder.ins().stack_addr(info.build_config.ptr_width.to_clif(), stack_slot, 0);
            Ok(addr)
        }
        _ => todo!(), // ERROR: unsupported op
    }
}
