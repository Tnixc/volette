use core::fmt;
use std::fmt::{Display, Formatter};

use generational_arena::{Arena, Index};
use string_interner::{StringInterner, backend::BucketBackend, symbol::SymbolUsize};

use crate::compiler::tokens::Span;

use super::node::{DefKind, ExprKind, Node, NodeKind, VType};

fn format_symbol(sym: SymbolUsize, interner: &StringInterner<BucketBackend<SymbolUsize>>) -> String {
    interner.resolve(sym).unwrap_or("<unknown>").to_string()
}

fn format_type_val(type_val: &VType, interner: &StringInterner<BucketBackend<SymbolUsize>>) -> String {
    match type_val {
        VType::Primitive(pt) => format!("Primitive{{type:\"{:?}\"}}", pt),
        VType::Custom(s_idx) => {
            format!("Custom{{name:\"{}\"}}", format_symbol(*s_idx, interner))
        }
        VType::Pointer(inner) => {
            format!("Pointer{{to:{}}}", format_type_val(inner, interner))
        }
    }
}

fn format_optional_type(opt_type: Option<VType>, interner: &StringInterner<BucketBackend<SymbolUsize>>) -> String {
    opt_type
        .as_ref()
        .map(|t| format_type_val(t, interner))
        .unwrap_or_else(|| "unresolved".to_string())
}

fn format_node_to_string(idx: Index, arena: &Arena<Node>, interner: &StringInterner<BucketBackend<SymbolUsize>>) -> String {
    arena
        .get(idx)
        .map(|node| node.to_json_like_string(arena, interner))
        .unwrap_or_else(|| "null".to_string())
}

fn format_optional_node_to_string(
    opt_idx: Option<Index>,
    arena: &Arena<Node>,
    interner: &StringInterner<BucketBackend<SymbolUsize>>,
) -> String {
    opt_idx
        .map(|idx| format_node_to_string(idx, arena, interner))
        .unwrap_or_else(|| "null".to_string())
}

fn format_node_list_to_string(indices: &[Index], arena: &Arena<Node>, interner: &StringInterner<BucketBackend<SymbolUsize>>) -> String {
    let items = indices
        .iter()
        .map(|idx| format_node_to_string(*idx, arena, interner))
        .collect::<Vec<_>>()
        .join(",");
    format!("[{}]", items)
}

impl Node {
    pub fn new(kind: NodeKind, span: Span) -> Self {
        Self { kind, span }
    }

    pub fn print_tree(&self, arena: &Arena<Node>, interner: &StringInterner<BucketBackend<SymbolUsize>>) {
        println!("{}", self.to_json_like_string(arena, interner));
    }

    // Kept if direct writing to a writer is still desired, otherwise print_tree is simpler.
    pub fn fmt_tree<W: std::io::Write>(
        &self,
        arena: &Arena<Node>,
        interner: &StringInterner<BucketBackend<SymbolUsize>>,
        out: &mut W,
    ) -> std::io::Result<()> {
        writeln!(out, "{}", self.to_json_like_string(arena, interner))
    }

    pub fn to_json_like_string(&self, arena: &Arena<Node>, interner: &StringInterner<BucketBackend<SymbolUsize>>) -> String {
        match &self.kind {
            NodeKind::Root { defs } => {
                format!("Root{{defs:{}}}", format_node_list_to_string(defs, arena, interner))
            }
            NodeKind::Expr { kind, type_ } => {
                format!(
                    "Expr{{type:{},kind:{}}}",
                    format_optional_type(type_.clone(), interner),
                    kind.to_json_like_string(arena, interner)
                )
            }
            NodeKind::Def { kind } => {
                format!("Def{{kind:{}}}", kind.to_json_like_string(arena, interner))
            }
        }
    }
}

impl Display for Node {
    /// does not provide meaningful output
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let arena = Arena::new();
        let interner = StringInterner::new();
        write!(f, "{}", self.to_json_like_string(&arena, &interner))
    }
}

impl ExprKind {
    fn to_json_like_string(&self, arena: &Arena<Node>, interner: &StringInterner<BucketBackend<SymbolUsize>>) -> String {
        match self {
            ExprKind::Literal(lit) => format!("Literal{{value:{:?}}}", lit),
            ExprKind::Identifier(sym) => {
                format!("Identifier{{name:\"{}\"}}", format_symbol(*sym, interner))
            }
            ExprKind::Assign { target, value } => format!(
                "Assign{{target:{},value:{}}}",
                format_node_to_string(*target, arena, interner),
                format_node_to_string(*value, arena, interner)
            ),
            ExprKind::CompoundAssign { target, value, op } => format!(
                "CompoundAssign{{op:\"{:?}\",target:{},value:{}}}",
                op,
                format_node_to_string(*target, arena, interner),
                format_node_to_string(*value, arena, interner)
            ),
            ExprKind::LetBinding {
                name,
                type_annotation,
                value,
            } => format!(
                "LetBinding{{name:\"{}\",type_annotation:{},value:{}}}",
                format_symbol(*name, interner),
                format_optional_type(type_annotation.clone(), interner),
                format_node_to_string(*value, arena, interner)
            ),
            ExprKind::BinOp { left, right, op } => format!(
                "BinOp{{op:\"{:?}\",left:{},right:{}}}",
                op,
                format_node_to_string(*left, arena, interner),
                format_node_to_string(*right, arena, interner)
            ),
            ExprKind::Call { func, args } => format!(
                "Call{{func:{},args:{}}}",
                format_node_to_string(*func, arena, interner),
                format_node_list_to_string(args, arena, interner)
            ),
            ExprKind::Block { exprs } => {
                format!("Block{{exprs:{}}}", format_node_list_to_string(exprs, arena, interner))
            }
            ExprKind::ParenExpr { expr } => {
                format!("ParenExpr{{expr:{}}}", format_node_to_string(*expr, arena, interner))
            }
            ExprKind::If {
                cond,
                then_block,
                else_block,
            } => format!(
                "If{{cond:{},then:{},else:{}}}",
                format_node_to_string(*cond, arena, interner),
                format_node_to_string(*then_block, arena, interner),
                format_optional_node_to_string(*else_block, arena, interner)
            ),
            ExprKind::UnaryOp { op, expr } => format!("UnaryOp{{op:\"{:?}\",expr:{}}}", op, format_node_to_string(*expr, arena, interner)),
            ExprKind::Cast { expr, target_type } => format!(
                "Cast{{expr:{},target_type:{}}}",
                format_node_to_string(*expr, arena, interner),
                format_type_val(target_type, interner)
            ),
            ExprKind::Return { value } => format!("Return{{value:{}}}", format_optional_node_to_string(*value, arena, interner)),
            ExprKind::Break => "Break{}".to_string(),
            ExprKind::While { cond, body } => format!(
                "While{{cond:{}}},{{body:{}}}",
                format_node_to_string(*cond, arena, interner),
                format_node_to_string(*body, arena, interner)
            ),
            ExprKind::BlockReturn { value } => format!("BlockReturn{{value:{}}}", format_optional_node_to_string(*value, arena, interner)),
        }
    }
}

impl DefKind {
    fn to_json_like_string(&self, arena: &Arena<Node>, interner: &StringInterner<BucketBackend<SymbolUsize>>) -> String {
        match self {
            DefKind::Function {
                name,
                params,
                body,
                return_type,
            } => {
                let params_str = params
                    .iter()
                    .map(|(p_name, p_type, _span)| {
                        format!(
                            "Param{{name:\"{}\",type:{}}}",
                            format_symbol(*p_name, interner),
                            format_type_val(p_type, interner)
                        )
                    })
                    .collect::<Vec<_>>()
                    .join(",");
                format!(
                    "Function{{name:\"{}\",params:[{}],body:{},return_type:{}}}",
                    format_symbol(*name, interner),
                    params_str,
                    format_node_to_string(*body, arena, interner),
                    format_type_val(return_type, interner)
                )
            }
            DefKind::Struct { name, fields } => {
                let fields_str = fields
                    .iter()
                    .map(|(f_name, f_type, _span)| {
                        format!(
                            "Field{{name:\"{}\",type:{}}}",
                            format_symbol(*f_name, interner),
                            format_type_val(f_type, interner)
                        )
                    })
                    .collect::<Vec<_>>()
                    .join(",");
                format!("Struct{{name:\"{}\",fields:[{}]}}", format_symbol(*name, interner), fields_str)
            }
        }
    }
}
