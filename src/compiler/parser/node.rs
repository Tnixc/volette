use generational_arena::Index;
use string_interner::symbol::SymbolUsize;

use crate::compiler::tokens::{PrimitiveTypes, Span};

#[derive(Debug)]
pub enum Type {
    Primitive(PrimitiveTypes),
    Custom(SymbolUsize),
}

#[derive(Debug)]
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
}

#[derive(Debug)]
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
}

#[derive(Debug)]
pub enum UnaryOpKind {
    Neg,
    Not,
}

#[derive(Debug)]
pub enum Literal {
    Int(i64),
    Float(f64),
    Bool(bool),
    Nil,
}

#[derive(Debug)]
pub enum NodeKind {
    Root {
        // imports, consts, etc
        defs: Vec<Index>,
    },
    Expr(Expr),
    Stmt(Stmt),
    Def(Def),
}

#[derive(Debug)]
pub struct Def {
    pub span: Span,
    pub kind: DefKind,
}

#[derive(Debug)]
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

#[derive(Debug)]
pub struct Stmt {
    pub span: Span,
    pub kind: StmtKind,
}

#[derive(Debug)]
pub enum StmtKind {
    Let {
        name: SymbolUsize,
        type_: Option<Type>,
        init_value: Option<Index>,
    },
    Return {
        value: Option<Index>,
    },
    Break,
    Loop {
        body: Index,
    },
}

#[derive(Debug)]
pub struct Expr {
    pub span: Span,
    pub kind: ExprKind,
    pub type_: Option<Type>,
}

#[derive(Debug)]
pub enum ExprKind {
    Literal(Literal),
    Identifier(SymbolUsize),
    Assign {
        name: SymbolUsize,
        value: Index,
    },
    BinOp {
        left: Index,
        right: Index,
        op: BinOpKind,
    },
    Call {
        func: Index,
        args: Vec<Index>,
    },
    Block {
        exprs: Vec<Index>,
    },
    If {
        cond: Index,
        then: Index,
        else_: Option<Index>,
    },
    UnaryOp {
        op: UnaryOpKind,
        expr: Index,
    },
    Cast {
        expr: Index,
        ty: Type,
    },
    Alloc {
        ty: Type,
    },
    Free {
        expr: Index,
    },
}
