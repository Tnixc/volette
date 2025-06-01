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
    AmpAmp,
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

    Amp,
    Pipe,
    PipePipe,

    FatArrow,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum PrimitiveTypes {
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
    Unit,
    Nil,
}

#[derive(Debug, Clone, Copy, PartialEq)]
#[allow(dead_code)]
pub enum TokenKind {
    IntLiteral(i64),
    FloatLiteral(f64),
    BoolLiteral(bool),
    StringLiteral(SymbolUsize),
    Keyword(Keyword),
    TypeLiteral(PrimitiveTypes),
    Punctuation(Punctuation),
    Identifier(SymbolUsize),
    Eof,
    Start,
}

/// A span of text in a file. Start and end are inclusive.
#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub struct Span {
    pub file: SymbolUsize,
    pub start: (usize, usize),
    pub end: (usize, usize),
}

#[derive(Debug, Clone, PartialEq, Copy)]
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
    pub fn new(file: SymbolUsize, start_line: usize, start_col: usize, end_line: usize, end_col: usize) -> Self {
        Self {
            file,
            start: (start_line, start_col),
            end: (end_line, end_col),
        }
    }

    fn connect(&self, other: &Self) -> ((usize, usize), (usize, usize)) {
        let start_line = self.start.0.min(other.start.0);
        let start_col = self.start.1.min(other.start.1);
        let end_line = self.end.0.max(other.end.0);
        let end_col = self.end.1.max(other.end.1);
        ((start_line, start_col), (end_line, end_col))
    }

    pub fn connect_mut(&mut self, other: &Self) -> &mut Self {
        let ((start_line, start_col), (end_line, end_col)) = self.connect(other);
        self.start = (start_line, start_col);
        self.end = (end_line, end_col);
        self
    }

    pub fn connect_new(&self, other: &Self) -> Self {
        let ((start_line, start_col), (end_line, end_col)) = self.connect(other);
        Self {
            file: self.file,
            start: (start_line, start_col),
            end: (end_line, end_col),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenName {
    Integer,
    Float,
    String,
    Keyword(Keyword),
    Punctuation(Punctuation),
    Identifier,
}

#[derive(Debug, Clone)]
pub struct DisplaySpan {
    pub file: String,
    pub start: (usize, usize),
    pub end: (usize, usize),
}
