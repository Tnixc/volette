use cranelift::prelude::{FunctionBuilder, Value};
use generational_arena::Index;

use crate::{
    SafeConvert,
    compiler::{
        analysis::literal_default_types,
        codegen::{Info, Scopes, error::TranslateError, unary_ops::expr_unaryop, variable::expr_assign},
        parser::node::{ExprKind, NodeKind, VType},
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
    scopes: &mut Scopes,
    info: &mut Info,
) -> Result<(Option<Value>, VType), TranslateError> {
    let node_idx = node; // Save the index before reassignment
    let node = info.nodes.get(node).safe();
    match &node.kind {
        NodeKind::Expr { kind, type_ } => {
            let value = match kind {
                ExprKind::Literal(literal) => match_literal(
                    *literal,
                    type_.clone().unwrap_or(literal_default_types(*literal)),
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
                } => Some(expr_if(node_idx, *cond, *then_block, *else_block, fn_builder, scopes, info)?),
                ExprKind::Assign { target, value } => Some(expr_assign(*target, *value, fn_builder, scopes, info)?),
                ExprKind::UnaryOp { op, expr } => Some(expr_unaryop(*op, *expr, fn_builder, scopes, info)?),
                _ => {
                    return Err(TranslateError::Unsupported {
                        what: format!("expression kind: {:?}", kind),
                        reason: "this expression type is not yet implemented".to_string(),
                        span: node.span.to_display(info.interner),
                    });
                }
            };

            let type_val = type_.clone().safe();
            match type_val {
                ty @ (VType::Primitive(PrimitiveTypes::Nil) | VType::Primitive(PrimitiveTypes::Never)) => Ok((None, ty)),
                ty => Ok((value, ty)),
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
