use cranelift::prelude::{FunctionBuilder, Value, Variable};
use generational_arena::Index;
use std::collections::HashMap;
use string_interner::symbol::SymbolUsize;

use crate::compiler::{
    analysis::literal_default_types,
    codegen::{Info, error::TranslateError},
    parser::node::{ExprKind, NodeKind, Type},
    tokens::PrimitiveTypes,
};

use super::{
    literal::match_literal,
    binary_ops::expr_binop,
    control_flow::expr_return,
    block::expr_block,
    function_call::expr_call,
    variable::{expr_let_binding, expr_identifier},
};

pub fn expr_to_val(
    node: Index,
    fn_builder: &mut FunctionBuilder,
    scopes: &mut Vec<HashMap<SymbolUsize, (Type, Variable)>>,
    info: &mut Info,
) -> Result<(Value, Type), TranslateError> {
    let node = info.nodes.get(node).expect("[Expr to val] Node index not found in arena");
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
                ExprKind::LetBinding { name, value, .. } => {
                    expr_let_binding(*name, *value, fn_builder, scopes, info)?
                }
                ExprKind::Identifier(sym) => {
                    expr_identifier(*sym, fn_builder, scopes)
                }
                ExprKind::Call { func, args } => {
                    expr_call(*func, args, fn_builder, scopes, info)?
                }
                _ => {
                    println!("valBefore: {:?} ||| {:?}", kind, type_);
                    todo!()
                }
            };

            println!("val: {:?} ||| {:?}", kind, type_);
            Ok((value, type_.unwrap_or(Type::Primitive(PrimitiveTypes::Never))))
            // TODO: Make the blocks accept -> statements like returns and eval its type instead of defaulting to Never
        }
        _ => todo!(),
    }
}