use cranelift::prelude::{FunctionBuilder, Value};
use generational_arena::Index;
use rootcause::prelude::*;

use crate::{
    SafeConvert,
    compiler::{
        analysis::literal_default_types,
        codegen::{Info, Scopes, unary_ops::expr_unaryop, variable::expr_assign},
        error::Help,
        parser::node::{ExprKind, NodeKind, VType},
        tokens::PrimitiveTypes,
    },
};

use super::{
    binary_ops::expr_binop,
    block::expr_block,
    cast::expr_cast,
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
) -> Result<(Option<Value>, VType), Report> {
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
                ExprKind::Cast { expr, target_type } => Some(expr_cast(*expr, target_type, fn_builder, scopes, info)?),
                _ => {
                    return Err(
                        crate::codegen_err!("Unsupported expression kind: {:?}", Some(node.span.to_display(info.interner)), kind)
                            .attach(Help("This feature is not yet implemented".into())),
                    );
                }
            };

            let type_val = type_.clone().safe();
            match type_val {
                ty @ (VType::Primitive(PrimitiveTypes::Nil) | VType::Primitive(PrimitiveTypes::Never)) => Ok((None, ty)),
                ty => Ok((value, ty)),
            }
        }
        _ => {
            return Err(crate::codegen_err!(
                "Invalid node: expected expression node in expr_to_val",
                Some(node.span.to_display(info.interner))
            )
            .attach(Help("This construct is not valid in the current context".into())));
        }
    }
}
