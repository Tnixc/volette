use generational_arena::{Arena, Index};
use string_interner::{StringInterner, backend::BucketBackend, symbol::SymbolUsize};

use super::node::{DefKind, ExprKind, Node, NodeKind};

/// Desugars compound assignments (+=, -=, etc.) into regular assignments with binary operations.
/// `x += 1` becomes `x = x + 1`
/// might have to break this up later
pub fn desugar(root: &mut Node, tree: &mut Arena<Node>, _interner: &StringInterner<BucketBackend<SymbolUsize>>) {
    match &root.kind {
        NodeKind::Root { defs } => {
            let defs_clone = defs.clone();
            for def_idx in defs_clone {
                desugar_def(def_idx, tree);
            }
        }
        _ => {}
    }
}

fn desugar_def(def_idx: Index, tree: &mut Arena<Node>) {
    let def_node = tree.get(def_idx).unwrap().clone();
    match &def_node.kind {
        NodeKind::Def { kind } => match kind {
            DefKind::Function { body, .. } => {
                desugar_expr(*body, tree);
            }
            DefKind::Struct { .. } => {}
        },
        _ => {}
    }
}

fn desugar_expr(expr_idx: Index, tree: &mut Arena<Node>) {
    let expr_node = tree.get(expr_idx).unwrap().clone();

    match &expr_node.kind {
        NodeKind::Expr { kind, .. } => {
            match kind {
                ExprKind::CompoundAssign { target, value, op } => {
                    // First desugar the value expression
                    desugar_expr(*value, tree);

                    // Get the target node to duplicate it for the BinOp's left operand
                    let target_node = tree.get(*target).unwrap().clone();

                    // Create a copy of the target for the BinOp's left side
                    let target_copy_idx = tree.insert(target_node.clone());

                    // Create the BinOp node: op(target_copy, value)
                    let binop_node = Node {
                        span: expr_node.span.clone(),
                        kind: NodeKind::Expr {
                            kind: ExprKind::BinOp {
                                left: target_copy_idx,
                                right: *value,
                                op: *op,
                            },
                            type_: None,
                        },
                    };
                    let binop_idx = tree.insert(binop_node);

                    // Replace the CompoundAssign with a regular Assign
                    let assign_node = tree.get_mut(expr_idx).unwrap();
                    assign_node.kind = NodeKind::Expr {
                        kind: ExprKind::Assign {
                            target: *target,
                            value: binop_idx,
                        },
                        type_: None,
                    };
                }
                ExprKind::BinOp { left, right, .. } => {
                    desugar_expr(*left, tree);
                    desugar_expr(*right, tree);
                }
                ExprKind::Assign { value, .. } => {
                    desugar_expr(*value, tree);
                }
                ExprKind::LetBinding { value, .. } => {
                    desugar_expr(*value, tree);
                }
                ExprKind::Call { func, args } => {
                    desugar_expr(*func, tree);
                    for arg in args {
                        desugar_expr(*arg, tree);
                    }
                }
                ExprKind::Block { exprs } => {
                    for expr in exprs {
                        desugar_expr(*expr, tree);
                    }
                }
                ExprKind::Return { value } => {
                    if let Some(v) = value {
                        desugar_expr(*v, tree);
                    }
                }
                ExprKind::BlockReturn { value } => {
                    if let Some(v) = value {
                        desugar_expr(*v, tree);
                    }
                }
                ExprKind::ParenExpr { expr } => {
                    desugar_expr(*expr, tree);
                }
                ExprKind::If {
                    cond,
                    then_block,
                    else_block,
                } => {
                    desugar_expr(*cond, tree);
                    desugar_expr(*then_block, tree);
                    if let Some(eb) = else_block {
                        desugar_expr(*eb, tree);
                    }
                }
                ExprKind::While { cond, body } => {
                    desugar_expr(*cond, tree);
                    desugar_expr(*body, tree);
                }
                ExprKind::Cast { expr, .. } => {
                    desugar_expr(*expr, tree);
                }
                ExprKind::UnaryOp { expr, .. } => {
                    desugar_expr(*expr, tree);
                }
                ExprKind::Literal(_) | ExprKind::Identifier(_) | ExprKind::Break => {}
            }
        }
        _ => {}
    }
}
