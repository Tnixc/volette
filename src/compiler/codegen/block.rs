use cranelift::prelude::{EntityRef, FunctionBuilder, Variable};
use std::collections::HashMap;
use string_interner::symbol::SymbolUsize;

use crate::compiler::{
    codegen::{error::TranslateError, expr::expr_to_val, Info},
    parser::node::{ExprKind, Node, NodeKind},
};

// TODO: generalize all expressions to be lowered as a expression

pub fn lower_block(
    node: &Node,
    fn_builder: &mut FunctionBuilder,
    info: &mut Info,
    scopes: &mut Vec<HashMap<SymbolUsize, Variable>>,
) -> Result<(), TranslateError> {
    let mut layer_scope: HashMap<SymbolUsize, Variable> = HashMap::new();
    match &node.kind {
        NodeKind::Expr { kind, type_ } => match kind {
            ExprKind::Block { exprs } => {
                for expr in exprs {
                    let expr = info
                        .nodes
                        .get(*expr)
                        .expect("[Lower block] Expr index not found in arena");
                    match &expr.kind {
                        NodeKind::Expr { kind: expr_kind, type_ } => match expr_kind {
                            ExprKind::LetBinding {
                                name,
                                type_annotation,
                                value,
                            } => {
                                if layer_scope.get(name).is_none() {
                                    let var = Variable::new(layer_scope.len());

                                    let (var_val, actual_type) =
                                        expr_to_val(*value, fn_builder, scopes, *type_annotation, info)?;

                                    let ty = actual_type.to_clif(info.build_config.ptr_width);

                                    println!("Declared var: {:?}", var);
                                    println!("Declared var val: {:?}", var_val);
                                    println!("Declared var type: {:?}", ty);

                                    fn_builder.declare_var(var, ty);
                                    fn_builder.def_var(var, var_val);

                                    layer_scope.insert(*name, var);
                                }
                            }
                            _ => todo!("Only let bindings are supported in block expressions rn"),
                        },
                        _ => unreachable!("Only exprs are supported in block expressions rn"),
                    }
                }
            }
            _ => unreachable!("Only block expressions are supported rn"),
        },
        _ => panic!("This is in lower_block, there should be only exprs in a block, this should be unreachable"),
    }
    scopes.push(layer_scope);
    Ok(())
}
