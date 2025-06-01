use generational_arena::{Arena, Index};
use std::fmt::{self, Display, Formatter};
use string_interner::{backend::BucketBackend, symbol::SymbolUsize, StringInterner};

use crate::compiler::tokens::{PrimitiveTypes, Span};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Type {
    Primitive(PrimitiveTypes),
    Custom(SymbolUsize),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Node {
    pub span: Span,
    pub kind: NodeKind,
    pub type_: Option<Type>,
}

impl Node {
    pub fn new(kind: NodeKind, span: Span) -> Self {
        Self {
            kind,
            span,
            type_: None,
        }
    }

    pub fn print_tree(&self, arena: &Arena<Node>, interner: &StringInterner<BucketBackend<SymbolUsize>>) {
        self.fmt_tree(arena, interner, 0, &mut std::io::stdout()).ok();
    }

    fn fmt_tree<W: std::io::Write>(
        &self,
        arena: &Arena<Node>,
        interner: &StringInterner<BucketBackend<SymbolUsize>>,
        indent: usize,
        out: &mut W,
    ) -> std::io::Result<()> {
        let indent_str = "  ".repeat(indent);
        writeln!(out, "{}{}", indent_str, self.display_node_kind(arena, interner, indent))?;
        Ok(())
    }

    fn display_node_kind(
        &self,
        arena: &Arena<Node>,
        interner: &StringInterner<BucketBackend<SymbolUsize>>,
        indent: usize,
    ) -> String {
        match &self.kind {
            NodeKind::Root { defs } => {
                let mut s = format!("Root [span: {:?}]", self.span.to_display(interner));
                for idx in defs {
                    if let Some(child) = arena.get(*idx) {
                        s.push('\n');
                        s.push_str(&child.display_node_kind_recursive(arena, interner, indent + 1));
                    }
                }
                s
            }
            NodeKind::Expr { kind, type_ } => {
                let mut s = format!("Expr [type: {:?}, span: {:?}]: ", type_, self.span.to_display(interner));
                s.push_str(&kind.display_expr_kind(arena, interner, indent));
                s
            }
            NodeKind::Stmt { kind } => {
                let mut s = format!("Stmt [span: {:?}]: ", self.span.to_display(interner));
                s.push_str(&kind.display_stmt_kind(arena, interner, indent));
                s
            }
            NodeKind::Def { kind } => {
                let mut s = format!("Def [span: {:?}]: ", self.span.to_display(interner));
                s.push_str(&kind.display_def_kind(arena, interner, indent));
                s
            }
        }
    }

    fn display_node_kind_recursive(
        &self,
        arena: &Arena<Node>,
        interner: &StringInterner<BucketBackend<SymbolUsize>>,
        indent: usize,
    ) -> String {
        let indent_str = "  ".repeat(indent);
        let mut s = format!("{}{}", indent_str, self.display_node_kind(arena, interner, indent));
        match &self.kind {
            NodeKind::Root { defs } => {
                for idx in defs {
                    if let Some(child) = arena.get(*idx) {
                        s.push('\n');
                        s.push_str(&child.display_node_kind_recursive(arena, interner, indent + 1));
                    }
                }
            }
            NodeKind::Expr { kind, .. } => {
                s.push_str(&kind.display_expr_children(arena, interner, indent + 1));
            }
            NodeKind::Stmt { kind } => {
                s.push_str(&kind.display_stmt_children(arena, interner, indent + 1));
            }
            NodeKind::Def { kind } => {
                s.push_str(&kind.display_def_children(arena, interner, indent + 1));
            }
        }
        s
    }
}

impl Display for Node {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            self.display_node_kind_recursive(
                // placeholders
                &Arena::new(),
                &StringInterner::new(),
                0
            )
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BinOpKind {
    Add,
    Sub,
    Mul,
    Div,
    Pow,
    Mod,
    Eq,
    NotEq,
    LessThan,
    LessThanOrEq,
    GreaterThan,
    GreaterThanOrEq,
    LogicalAnd,
    LogicalOr,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum UnaryOpKind {
    Neg,
    Not,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Literal {
    Int(i64),
    Float(f64),
    Bool(bool),
    Nil,
}

#[derive(Debug, Clone, PartialEq)]
pub enum NodeKind {
    Root {
        // imports, consts, etc
        defs: Vec<Index>,
    },
    Expr {
        kind: ExprKind,
        type_: Option<Type>,
    },
    Stmt {
        kind: StmtKind,
    },
    Def {
        kind: DefKind,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Def {
    pub span: Span,
    pub kind: DefKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum DefKind {
    Function {
        name: SymbolUsize,
        params: Vec<(SymbolUsize, Type, Span)>,

        /// Block expr
        body: Index,

        return_type: Type,
    },
    Struct {
        name: SymbolUsize,
        fields: Vec<(SymbolUsize, Type, Span)>,
    },
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum StmtKind {
    Return { value: Option<Index> },
    BlockReturn { value: Option<Index> },
    Break,
    Loop { body: Index },
    ExpressionStmt { expr: Index },
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    Literal(Literal),
    Identifier(SymbolUsize),
    Assign {
        target: Index, // LHS expression (e.g. identifier, field access, array index)
        value: Index,  // RHS expression
    },
    LetBinding {
        // For `let name = value` expressions
        name: SymbolUsize,
        type_annotation: Option<Type>,
        value: Index, // Initializer is mandatory for the expression to have this value
    },
    BinOp {
        left: Index,
        right: Index,
        op: BinOpKind,
    },
    Call {
        func: Index, // Can be an identifier or any expression that evaluates to a function
        args: Vec<Index>,
    },
    Block {
        // Block can be an expression, its value is the last expression in it
        // Or an explicit `=> value` syntax from your spec
        exprs: Vec<Index>, // Sequence of statements/expressions
    },
    ParenExpr {
        expr: Index,
    },
    If {
        cond: Index,
        then_block: Index,         // Should be a block expression
        else_block: Option<Index>, // Should be a block expression
    },
    UnaryOp {
        op: UnaryOpKind,
        expr: Index,
    },
    Cast {
        expr: Index,
        target_type: Type,
    },
    // Alloc {
    //     target_type: Type,
    // },
    // Free {
    //     expr: Index,
    // },
    // TODO: FieldAccess, MethodCall
}

impl ExprKind {
    fn display_expr_kind(
        &self,
        arena: &Arena<Node>,
        interner: &StringInterner<BucketBackend<SymbolUsize>>,
        indent: usize,
    ) -> String {
        match self {
            ExprKind::Literal(lit) => format!("Literal({:?})", lit),
            ExprKind::Identifier(sym) => {
                let name = interner.resolve(*sym).unwrap_or("<unknown>");
                format!("Identifier({})", name)
            }
            ExprKind::Assign { target, value } => {
                format!("Assign(target: {:?}, value: {:?})", target, value)
            }
            ExprKind::LetBinding {
                name,
                type_annotation,
                value,
            } => {
                let n = interner.resolve(*name).unwrap_or("<unknown>");
                format!(
                    "LetBinding(name: {}, type: {:?}, value: {:?})",
                    n, type_annotation, value
                )
            }
            ExprKind::BinOp { left, right, op } => {
                format!("BinOp({:?}, left: {:?}, right: {:?})", op, left, right)
            }
            ExprKind::Call { func, args } => {
                format!("Call(func: {:?}, args: {:?})", func, args)
            }
            ExprKind::Block { exprs } => {
                format!("Block({} exprs)", exprs.len())
            }
            ExprKind::ParenExpr { expr } => {
                format!("ParenExpr(expr: {:?})", expr)
            }
            ExprKind::If {
                cond,
                then_block,
                else_block,
            } => {
                format!("If(cond: {:?}, then: {:?}, else: {:?})", cond, then_block, else_block)
            }
            ExprKind::UnaryOp { op, expr } => {
                format!("UnaryOp({:?}, expr: {:?})", op, expr)
            }
            ExprKind::Cast { expr, target_type } => {
                format!("Cast(expr: {:?}, type: {:?})", expr, target_type)
            }
        }
    }

    fn display_expr_children(
        &self,
        arena: &Arena<Node>,
        interner: &StringInterner<BucketBackend<SymbolUsize>>,
        indent: usize,
    ) -> String {
        let mut s = String::new();
        match self {
            ExprKind::Assign { target, value } => {
                if let Some(t) = arena.get(*target) {
                    s.push('\n');
                    s.push_str(&t.display_node_kind_recursive(arena, interner, indent));
                }
                if let Some(v) = arena.get(*value) {
                    s.push('\n');
                    s.push_str(&v.display_node_kind_recursive(arena, interner, indent));
                }
            }
            ExprKind::LetBinding { value, .. } => {
                if let Some(v) = arena.get(*value) {
                    s.push('\n');
                    s.push_str(&v.display_node_kind_recursive(arena, interner, indent));
                }
            }
            ExprKind::BinOp { left, right, .. } => {
                if let Some(l) = arena.get(*left) {
                    s.push('\n');
                    s.push_str(&l.display_node_kind_recursive(arena, interner, indent));
                }
                if let Some(r) = arena.get(*right) {
                    s.push('\n');
                    s.push_str(&r.display_node_kind_recursive(arena, interner, indent));
                }
            }
            ExprKind::Call { func, args } => {
                if let Some(f) = arena.get(*func) {
                    s.push('\n');
                    s.push_str(&f.display_node_kind_recursive(arena, interner, indent));
                }
                for arg in args {
                    if let Some(a) = arena.get(*arg) {
                        s.push('\n');
                        s.push_str(&a.display_node_kind_recursive(arena, interner, indent));
                    }
                }
            }
            ExprKind::Block { exprs } => {
                for idx in exprs {
                    if let Some(e) = arena.get(*idx) {
                        s.push('\n');
                        s.push_str(&e.display_node_kind_recursive(arena, interner, indent));
                    }
                }
            }
            ExprKind::ParenExpr { expr } => {
                if let Some(e) = arena.get(*expr) {
                    s.push('\n');
                    s.push_str(&e.display_node_kind_recursive(arena, interner, indent));
                }
            }
            ExprKind::If {
                cond,
                then_block,
                else_block,
            } => {
                if let Some(c) = arena.get(*cond) {
                    s.push('\n');
                    s.push_str(&c.display_node_kind_recursive(arena, interner, indent));
                }
                if let Some(t) = arena.get(*then_block) {
                    s.push('\n');
                    s.push_str(&t.display_node_kind_recursive(arena, interner, indent));
                }
                if let Some(Some(e)) = else_block.as_ref().map(|idx| arena.get(*idx)) {
                    s.push('\n');
                    s.push_str(&e.display_node_kind_recursive(arena, interner, indent));
                }
            }
            ExprKind::UnaryOp { expr, .. } => {
                if let Some(e) = arena.get(*expr) {
                    s.push('\n');
                    s.push_str(&e.display_node_kind_recursive(arena, interner, indent));
                }
            }
            ExprKind::Cast { expr, .. } => {
                if let Some(e) = arena.get(*expr) {
                    s.push('\n');
                    s.push_str(&e.display_node_kind_recursive(arena, interner, indent));
                }
            }
            _ => {}
        }
        s
    }
}

impl StmtKind {
    fn display_stmt_kind(
        &self,
        arena: &Arena<Node>,
        interner: &StringInterner<BucketBackend<SymbolUsize>>,
        _indent: usize,
    ) -> String {
        match self {
            StmtKind::Return { value } => format!("Return({:?})", value),
            StmtKind::BlockReturn { value } => format!("BlockReturn({:?})", value),
            StmtKind::Break => "Break".to_string(),
            StmtKind::Loop { body } => format!("Loop(body: {:?})", body),
            StmtKind::ExpressionStmt { expr } => format!("ExpressionStmt(expr: {:?})", expr),
        }
    }

    fn display_stmt_children(
        &self,
        arena: &Arena<Node>,
        interner: &StringInterner<BucketBackend<SymbolUsize>>,
        indent: usize,
    ) -> String {
        let mut s = String::new();
        match self {
            StmtKind::Return { value } | StmtKind::BlockReturn { value } => {
                if let Some(idx) = value {
                    if let Some(n) = arena.get(*idx) {
                        s.push('\n');
                        s.push_str(&n.display_node_kind_recursive(arena, interner, indent));
                    }
                }
            }
            StmtKind::Loop { body } => {
                if let Some(b) = arena.get(*body) {
                    s.push('\n');
                    s.push_str(&b.display_node_kind_recursive(arena, interner, indent));
                }
            }
            StmtKind::ExpressionStmt { expr } => {
                if let Some(e) = arena.get(*expr) {
                    s.push('\n');
                    s.push_str(&e.display_node_kind_recursive(arena, interner, indent));
                }
            }
            _ => {}
        }
        s
    }
}

impl DefKind {
    fn display_def_kind(
        &self,
        _arena: &Arena<Node>,
        interner: &StringInterner<BucketBackend<SymbolUsize>>,
        _indent: usize,
    ) -> String {
        match self {
            DefKind::Function {
                name,
                params,
                body,
                return_type,
            } => {
                let n = interner.resolve(*name).unwrap_or("<unknown>");
                let params_str = params
                    .iter()
                    .map(|(n, t, _)| {
                        let pname = interner.resolve(*n).unwrap_or("<unknown>");
                        format!("{}: {:?}", pname, t)
                    })
                    .collect::<Vec<_>>()
                    .join(", ");
                format!(
                    "Function(name: {}, params: [{}], body: {:?}, return_type: {:?})",
                    n, params_str, body, return_type
                )
            }
            DefKind::Struct { name, fields } => {
                let n = interner.resolve(*name).unwrap_or("<unknown>");
                let fields_str = fields
                    .iter()
                    .map(|(n, t, _)| {
                        let fname = interner.resolve(*n).unwrap_or("<unknown>");
                        format!("{}: {:?}", fname, t)
                    })
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("Struct(name: {}, fields: [{}])", n, fields_str)
            }
        }
    }

    fn display_def_children(
        &self,
        arena: &Arena<Node>,
        interner: &StringInterner<BucketBackend<SymbolUsize>>,
        indent: usize,
    ) -> String {
        let mut s = String::new();
        match self {
            DefKind::Function { body, .. } => {
                if let Some(b) = arena.get(*body) {
                    s.push('\n');
                    s.push_str(&b.display_node_kind_recursive(arena, interner, indent));
                }
            }
            _ => {}
        }
        s
    }
}
