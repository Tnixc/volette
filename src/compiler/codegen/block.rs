use std::hash::Hash;

use cranelift::{
    codegen::Context,
    object::ObjectModule,
    prelude::{EntityRef, FunctionBuilder, Variable},
};
use generational_arena::Arena;
use hashbrown::HashMap;
use string_interner::{backend::BucketBackend, symbol::SymbolUsize, StringInterner};

use crate::compiler::{
    codegen::{error::TranslateError, BuildConfig},
    parser::node::{ExprKind, Node, NodeKind},
};

pub fn lower_block(
    node: &Node,
    interner: StringInterner<BucketBackend<SymbolUsize>>,
    fn_builder: &mut FunctionBuilder,
    build_config: &BuildConfig,
    nodes: &Arena<Node>,
    scopes: &mut Vec<HashMap<SymbolUsize, Variable>>,
) -> Result<(), TranslateError> {
    let layer_scope: HashMap<SymbolUsize, Variable> = HashMap::new();
    match &node.kind {
        NodeKind::Expr { kind, type_ } => match kind {
            ExprKind::Block { exprs } => {
                for expr in exprs {
                    let expr = nodes.get(*expr).expect("[Lower block] Expr index not found in arena");
                    match &expr.kind {
                        NodeKind::Expr { kind: expr_kind, type_ } => match expr_kind {
                            ExprKind::LetBinding {
                                name,
                                type_annotation,
                                value,
                            } => {
                                if layer_scope.get(name).is_none() {
                                    let var = Variable::new(layer_scope.len());
                                    let ty = type_annotation
                                        .expect("No type inference!")
                                        .to_clif(build_config.ptr_width);
                                    fn_builder.declare_var(var, ty);
                                    // fn_builder.def_var(var, val);
                                }
                            }
                            _ => todo!(),
                        },
                        _ => unreachable!(),
                    }
                }
            }
            _ => unreachable!(),
        },
        _ => unreachable!(),
    }
    Ok(())
}
