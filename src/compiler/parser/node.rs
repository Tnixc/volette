use cranelift::prelude::types;
use derive_more::Display;
use generational_arena::Index;
use string_interner::symbol::SymbolUsize;

use crate::compiler::{
    codegen::PtrWidth,
    tokens::{PrimitiveTypes, Span},
};

#[derive(Debug, Clone, PartialEq, Display)]
pub enum VType {
    #[display("{:?}", _0)]
    Primitive(PrimitiveTypes),
    #[display("Custom({:?})", _0)]
    Custom(SymbolUsize),
    #[display("Pointer({:?})", _0)]
    Pointer(Box<VType>),
}

impl VType {
    pub fn to_clif(&self, ptr_bits: PtrWidth) -> types::Type {
        match self {
            VType::Primitive(pt) => pt.to_clif(ptr_bits),
            VType::Custom(_) => panic!("Custom types are not yet supported in codegen"),
            VType::Pointer(_) => match ptr_bits {
                PtrWidth::X64 => types::I64,
                PtrWidth::X32 => types::I32,
            }, // yeah idk
        }
    }

    pub fn coerced(&self) -> &VType {
        match self {
            // VType::Pointer(_) => match ptr_width() {
            //     // PtrWidth::X64 => &VType::Primitive(PrimitiveTypes::IS),
            //     // PtrWidth::X32 => &VType::Primitive(PrimitiveTypes::I32),
            //     //
            // }, // yeah idk
            VType::Pointer(_) => &VType::Primitive(PrimitiveTypes::Usize),
            _ => self,
        }
    }
}

#[macro_export]
macro_rules! is_int {
    () => {
        crate::compiler::tokens::PrimitiveTypes::I8
            | crate::compiler::tokens::PrimitiveTypes::I16
            | crate::compiler::tokens::PrimitiveTypes::I32
            | crate::compiler::tokens::PrimitiveTypes::I64
            | crate::compiler::tokens::PrimitiveTypes::U8
            | crate::compiler::tokens::PrimitiveTypes::U16
            | crate::compiler::tokens::PrimitiveTypes::U32
            | crate::compiler::tokens::PrimitiveTypes::U64
            | crate::compiler::tokens::PrimitiveTypes::Usize
            | crate::compiler::tokens::PrimitiveTypes::Isize
    };
}

#[macro_export]
macro_rules! is_float {
    () => {
        crate::compiler::tokens::PrimitiveTypes::F32 | crate::compiler::tokens::PrimitiveTypes::F64
    };
}

impl PrimitiveTypes {
    pub fn is_signed(&self) -> bool {
        matches!(
            self,
            PrimitiveTypes::I8 | PrimitiveTypes::I16 | PrimitiveTypes::I32 | PrimitiveTypes::I64 | PrimitiveTypes::Isize
        )
    }

    pub fn is_unsigned(&self) -> bool {
        matches!(
            self,
            PrimitiveTypes::U8 | PrimitiveTypes::U16 | PrimitiveTypes::U32 | PrimitiveTypes::U64 | PrimitiveTypes::Usize
        )
    }

    pub fn is_int(&self) -> bool {
        matches!(self, is_int!())
    }

    pub fn is_float(&self) -> bool {
        matches!(self, is_float!())
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
        type_: Option<VType>, // This is NodeKind::Expr.type_
    },
    Def {
        kind: DefKind,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Display)]
pub enum BinOpKind {
    #[display("+")]
    Add,
    #[display("-")]
    Sub,
    #[display("*")]
    Mul,
    #[display("/")]
    Div,
    #[display("**")]
    Pow,
    #[display("%")]
    Mod,
    #[display("==")]
    Eq,
    #[display("!=")]
    NotEq,
    #[display("<")]
    LessThan,
    #[display("<=")]
    LessThanOrEq,
    #[display(">")]
    GreaterThan,
    #[display(">=")]
    GreaterThanOrEq,
    #[display("&&")]
    LogicalAnd,
    #[display("||")]
    LogicalOr,
    #[display("^")]
    BitwiseXor,
    #[display("&")]
    BitwiseAnd,
    #[display("|")]
    BitwiseOr,
    #[display("<<")]
    BitwiseShLeft,
    #[display(">>")]
    BitwiseShRight,
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
    CompoundAssign {
        target: Index,
        value: Index,
        op: BinOpKind,
    },
    LetBinding {
        name: SymbolUsize,
        type_annotation: Option<VType>,
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
    While {
        cond: Index,
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
        target_type: VType,
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
        params: Vec<(SymbolUsize, VType, Span)>,
        body: Index,
        return_type: VType,
    },
    Struct {
        name: SymbolUsize,
        fields: Vec<(SymbolUsize, VType, Span)>,
    },
}

impl Node {
    pub fn set_type(&mut self, t: VType) {
        match &mut self.kind {
            NodeKind::Expr { type_, .. } => {
                type_.replace(t);
            }
            _ => unimplemented!(),
        }
    }
}
