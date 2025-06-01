use generational_arena::Index;
use string_interner::symbol::SymbolUsize;

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
