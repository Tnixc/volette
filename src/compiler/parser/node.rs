use cranelift::prelude::types;
use generational_arena::Index;
use string_interner::symbol::SymbolUsize;

use crate::compiler::{
    codegen::PtrWidth,
    tokens::{PrimitiveTypes, Span},
};

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Primitive(PrimitiveTypes),
    Custom(SymbolUsize),
    Pointer(Box<Type>),
}

impl Type {
    pub fn to_clif(&self, ptr_bits: PtrWidth) -> types::Type {
        match self {
            Type::Primitive(pt) => pt.to_clif(ptr_bits),
            Type::Custom(_) => panic!("Custom types are not yet supported in codegen"),
            Type::Pointer(_) => match ptr_bits {
                PtrWidth::X64 => types::I64,
                PtrWidth::X32 => types::I32,
            }, // yeah idk
        }
    }
}

impl PrimitiveTypes {
    pub fn is_integer(&self) -> bool {
        matches!(
            self,
            PrimitiveTypes::I8
                | PrimitiveTypes::I16
                | PrimitiveTypes::I32
                | PrimitiveTypes::I64
                | PrimitiveTypes::U8
                | PrimitiveTypes::U16
                | PrimitiveTypes::U32
                | PrimitiveTypes::U64
        )
    }

    pub fn is_float(&self) -> bool {
        matches!(self, PrimitiveTypes::F32 | PrimitiveTypes::F64)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Node {
    pub span: Span,
    pub kind: NodeKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum NodeKind {
    Root {
        defs: Vec<Index>,
    },
    Expr {
        kind: ExprKind,
        type_: Option<Type>, // This is NodeKind::Expr.type_
    },
    Def {
        kind: DefKind,
    },
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
    BitwiseXor,
    BitwiseAnd,
    BitwiseOr,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum UnaryOpKind {
    Neg,
    Not,
    Deref,     // @ptr
    AddressOf, // &var
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Literal {
    Int(i64),
    Float(f64),
    Bool(bool),
    Nil,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    Literal(Literal),
    Identifier(SymbolUsize),
    BinOp {
        left: Index,
        right: Index,
        op: BinOpKind,
    },
    Assign {
        target: Index,
        value: Index,
    },
    LetBinding {
        name: SymbolUsize,
        type_annotation: Option<Type>,
        value: Index,
    },
    Call {
        func: Index,
        args: Vec<Index>,
    },
    Block {
        exprs: Vec<Index>,
    },
    Return {
        value: Option<Index>,
    },
    Break,
    Loop {
        body: Index,
    },
    BlockReturn {
        value: Option<Index>,
    },
    ParenExpr {
        expr: Index,
    },
    If {
        cond: Index,
        then_block: Index,
        else_block: Option<Index>,
    },
    Cast {
        expr: Index,
        target_type: Type,
    },
    UnaryOp {
        op: UnaryOpKind,
        expr: Index,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum DefKind {
    Function {
        name: SymbolUsize,
        params: Vec<(SymbolUsize, Type, Span)>,
        body: Index,
        return_type: Type,
    },
    Struct {
        name: SymbolUsize,
        fields: Vec<(SymbolUsize, Type, Span)>,
    },
}

impl Node {
    pub fn set_type(&mut self, t: Type) {
        match &mut self.kind {
            NodeKind::Expr { type_, .. } => {
                type_.replace(t);
            }
            _ => unimplemented!(),
        }
    }
}
