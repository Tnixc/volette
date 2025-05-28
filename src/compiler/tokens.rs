use std::path::PathBuf;

use string_interner::symbol::SymbolUsize;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Keyword {
    Fn,
    Use,
    Const,
    Let,
    Loop,
    Break,
    Return,
    Struct,
    Alloc,
    Free,
    Pub,
    Local,
    Self_,
    As,
    In,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Punctuation {
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    OpenBracket,
    CloseBracket,
    Comma,
    Dot,
    Colon,
    Semicolon,
    And,
    Bang,

    Plus,
    Minus, // Binop and Unary
    Star,
    StarStar,
    Slash,
    Percent,
    Eq,
    EqEq,
    NotEq,
    LessThan, // generics ahh
    LessThanOrEq,
    GreaterThan,
    GreaterThanOrEq,

    Ampersand,
    PipePipe,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Type {
    U8,
    U16,
    U32,
    U64,
    I8,
    I16,
    I32,
    I64,
    F32,
    F64,
    Bool,
    None,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenKind {
    IntLiteral(i64),
    FloatLiteral(f64),
    StringLiteral(SymbolUsize),
    Keyword(Keyword),
    TypeLiteral(Type),
    Punctuation(Punctuation),
    Identifier(SymbolUsize),
}

/// A span of text in a file. Start and end are inclusive.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Span {
    pub file: SymbolUsize,
    pub line: usize,
    pub start: usize,
    pub end: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    pub fn new(kind: TokenKind, span: Span) -> Self {
        Self { kind, span }
    }
}

impl Span {
    pub fn new(file: SymbolUsize, line: usize, start: usize, end: usize) -> Self {
        Self { file, line, start, end }
    }
}
