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
    Ampersand,

    Plus,
    Minus, // Binop and Unary
    Star,
    Pow,
    Slash,
    Percent,
    Equal,
    EqualEqual,
    NotEqual,
    LessThan, // generics ahh
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,

    // Logical operators
    And,
    Or,
    Xor,
    Not, // Unary
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenKind {
    IntLiteral(i64),
    FloatLiteral(f64),
    StringLiteral(SymbolUsize),
    Keyword(Keyword),
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
    kind: TokenKind,
    span: Span,
}

impl Token {
    pub fn new(kind: TokenKind, span: Span) -> Self {
        Self { kind, span }
    }
}

impl Span {
    pub fn new(file: SymbolUsize, line: usize, start: usize, end: usize) -> Self {
        Self {
            file,
            line,
            start,
            end,
        }
    }
}
