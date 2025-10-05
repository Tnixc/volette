use cranelift::prelude::{FunctionBuilder, Value, Variable};
use generational_arena::Index;
use std::collections::HashMap;
use string_interner::symbol::SymbolUsize;

use crate::{
    SafeConvert,
    compiler::{
        analysis::literal_default_types,
        codegen::{Info, error::TranslateError},
        parser::node::{ExprKind, NodeKind, Type},
        tokens::PrimitiveTypes,
    },
};

use super::{
    binary_ops::expr_binop,
    block::expr_block,
    control_flow::{expr_if, expr_return},
    function_call::expr_call,
    literal::match_literal,
    variable::{expr_identifier, expr_let_binding},
};

pub fn expr_to_val(
    node: Index,
    fn_builder: &mut FunctionBuilder,
    scopes: &mut Vec<HashMap<SymbolUsize, (Type, Variable)>>,
    info: &mut Info,
) -> Result<(Value, Type), TranslateError> {
    let node = info.nodes.get(node).safe();
    match &node.kind {
        NodeKind::Expr { kind, type_ } => {
            let value = match kind {
                ExprKind::Literal(literal) => match_literal(*literal, type_.unwrap_or(literal_default_types(*literal)), node, fn_builder)?,
                ExprKind::BinOp { left, right, op } => expr_binop(*left, *right, *op, fn_builder, scopes, info)?,
                ExprKind::Return { value: ret_val } => expr_return(*ret_val, fn_builder, scopes, info)?,
                ExprKind::Block { exprs } => {
                    let (val, _) = expr_block(exprs, fn_builder, scopes, info)?;
                    val
                }
                ExprKind::LetBinding { name, value, .. } => expr_let_binding(*name, *value, fn_builder, scopes, info)?,
                ExprKind::Identifier(sym) => expr_identifier(*sym, fn_builder, scopes)?,
                ExprKind::Call { func, args } => expr_call(*func, args, fn_builder, scopes, info)?,
                ExprKind::If {
                    cond,
                    then_block,
                    else_block,
                } => expr_if(*cond, *then_block, *else_block, fn_builder, scopes, info)?,
                _ => {
                    return Err(TranslateError::Internal(format!("Unsupported expression kind: {:?}", kind)));
                }
            };

            Ok((value, type_.unwrap()))
            // TODO: Make the blocks accept -> statements like returns and eval its type instead of defaulting to Never
        }
        _ => return Err(TranslateError::Internal("Expected expression node in expr_to_val".to_string())),
    }
}
