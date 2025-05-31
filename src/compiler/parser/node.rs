use generational_arena::Index;
use string_interner::symbol::SymbolUsize;

use crate::compiler::tokens::{PrimitiveTypes, Span};

pub enum Type {
    Primitive(PrimitiveTypes),
    Custom(SymbolUsize),
}

pub struct Node {
    pub span: Span,
    pub kind: NodeKind,
    pub type_: Type,
}

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

pub enum UnaryOpKind {
    Neg,
    Not,
}

pub enum Literal {
    I64(i64),
    I32(i32),
    I16(i16),
    I8(i8),
    U64(u64),
    U32(u32),
    U16(u16),
    U8(u8),
    F64(f64),
    Bool(bool),
    None,
}

pub enum NodeKind {
    Expr(ExprKind),
    Stmt(StmtKind),
    Def(DefKind),
}

pub enum DefKind {
    Function {
        name: SymbolUsize,
        params: Vec<(SymbolUsize, Type)>,

        /// Block expr
        body: Index,

        return_type: Type,
    },
    Struct {
        name: SymbolUsize,
        fields: Vec<(SymbolUsize, Type)>,
    },
}

pub enum StmtKind {
    Expr(ExprKind),
    Let { name: SymbolUsize, value: Index },
    Return { value: Option<Index> },
    Break,
    Loop { body: Index },
}

pub enum ExprKind {
    Literal(Literal),
    Identifier(SymbolUsize),
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
