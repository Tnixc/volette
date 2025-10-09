use cranelift::prelude::{FunctionBuilder, Value, Variable};
use generational_arena::Index;
use std::collections::HashMap;
use string_interner::symbol::SymbolUsize;

use crate::{
    SafeConvert,
    compiler::{
        analysis::literal_default_types,
        codegen::{Info, error::TranslateError, variable::expr_assign},
        parser::node::{ExprKind, NodeKind, Type},
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
) -> Result<(Option<Value>, Type), TranslateError> {
    let node = info.nodes.get(node).safe();
    match &node.kind {
        NodeKind::Expr { kind, type_ } => {
            let value = match kind {
                ExprKind::Literal(literal) => match_literal(
                    *literal,
                    type_.unwrap_or(literal_default_types(*literal)),
                    node,
                    fn_builder,
                    info.interner,
                )?,
                ExprKind::BinOp { left, right, op } => Some(expr_binop(*left, *right, *op, fn_builder, scopes, info)?),
                ExprKind::Return { value: ret_val } => expr_return(*ret_val, fn_builder, scopes, info)?,
                ExprKind::Block { exprs } => Some(expr_block(exprs, fn_builder, scopes, info)?),
                ExprKind::LetBinding { name, value, .. } => Some(expr_let_binding(*name, *value, fn_builder, scopes, info)?),
                ExprKind::Identifier(sym) => Some(expr_identifier(*sym, fn_builder, scopes, info)?),
                ExprKind::Call { func, args } => Some(expr_call(*func, args, fn_builder, scopes, info)?),
                ExprKind::If {
                    cond,
                    then_block,
                    else_block,
                } => Some(expr_if(*cond, *then_block, *else_block, fn_builder, scopes, info)?),
                ExprKind::Assign { target, value } => Some(expr_assign(*target, *value, fn_builder, scopes, info)?),
                _ => {
                    return Err(TranslateError::Unsupported {
                        what: format!("expression kind: {:?}", kind),
                        reason: "this expression type is not yet implemented".to_string(),
                        span: node.span.to_display(info.interner),
                    });
                }
            };

            match type_.safe() {
                Type::Primitive(crate::compiler::tokens::PrimitiveTypes::Nil)
                | Type::Primitive(crate::compiler::tokens::PrimitiveTypes::Never) => Ok((None, type_.safe())),
                _ => Ok((value, type_.safe())),
            }
        }
        _ => {
            return Err(TranslateError::Invalid {
                what: "node".to_string(),
                reason: "expected expression node in expr_to_val".to_string(),
                span: node.span.to_display(info.interner),
            });
        }
    }
}
