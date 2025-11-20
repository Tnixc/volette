use crate::compiler::tokens::PrimitiveTypes;
use crate::{is_float, is_int};
use cranelift::prelude::{FunctionBuilder, IntCC, MemFlags, StackSlotData, StackSlotKind};
use cranelift::prelude::{InstBuilder, Value};
use generational_arena::Index;

use crate::{
    SafeConvert,
    compiler::{
        codegen::{Info, Scopes, error::TranslateError, expr::expr_to_val, ptr_width},
        parser::node::{ExprKind, NodeKind, UnaryOpKind, VType},
    },
};

pub fn expr_unaryop(
    op: UnaryOpKind,
    expr: Index,
    fn_builder: &mut FunctionBuilder,
    scopes: &mut Scopes,
    info: &mut Info,
) -> Result<Value, TranslateError> {
    if op == UnaryOpKind::AddressOf {
        let node = info.nodes.get(expr).safe();
        if let NodeKind::Expr {
            kind: ExprKind::Identifier(sym),
            ..
        } = &node.kind
        {
            let slot = scopes
                .iter()
                .rev()
                .find_map(|scope| scope.get(sym))
                .map(|(_, slot)| *slot)
                .expect("address of undeclared variable");
            let addr = fn_builder.ins().stack_addr(ptr_width().to_clif(), slot, 0);
            return Ok(addr);
        }
    }

    let (item, item_type) = expr_to_val(expr, fn_builder, scopes, info)?;
    if item.is_none() {};
    let item = item.safe();
    match (op, item_type) {
        (UnaryOpKind::Neg, VType::Primitive(is_int!())) => Ok(fn_builder.ins().ineg(item)),
        (UnaryOpKind::Neg, VType::Primitive(is_float!())) => Ok(fn_builder.ins().fneg(item)),
        (UnaryOpKind::Not, VType::Primitive(PrimitiveTypes::Bool)) => Ok(fn_builder.ins().icmp_imm(IntCC::Equal, item, 0)),
        (UnaryOpKind::AddressOf, VType::Primitive(ty)) => {
            let size = ty.to_clif(ptr_width()).bytes();
            let stack_slot = fn_builder.create_sized_stack_slot(StackSlotData::new(StackSlotKind::ExplicitSlot, size, 0));
            fn_builder.ins().stack_store(item, stack_slot, 0);
            let addr = fn_builder.ins().stack_addr(ptr_width().to_clif(), stack_slot, 0);
            Ok(addr)
        }
        (UnaryOpKind::Deref, VType::Pointer(ty)) => {
            let ty = ty.to_clif(ptr_width());
            let loaded = fn_builder.ins().load(ty, MemFlags::new(), item, 0);
            Ok(loaded)
        }
        _ => todo!(),
    }
}
