use core::fmt;
use std::fmt::Display;

use thiserror::Error;

use crate::compiler::tokens::{DisplaySpan, Token, TokenKind};

#[derive(Error, Debug, Clone)]
pub enum ParserError {
    #[error("Unexpected EOF")]
    UnexpectedEof { token: Token },

    #[error("Unexpected token at top level: {token}")]
    UnexpectedTokenAtTopLevel { token: Token },

    #[error("Unexpected token: {token}, expecting one of {allowed:?}")]
    UnexpectedToken { token: Token, allowed: Vec<TokenKind> },

    #[error("Expected a type literal or an identifier for a type: {token}")]
    ExpectedType { token: Token },

    #[error("Identifier expected after 'fn'")]
    IdentifierExpectedAfterFn { token: Token },

    #[error("Open parenthesis expected after function name: {token}")]
    OpenParenExpectedAfterFnName { token: Token },

    #[error("Function parameter name expected: {token}")]
    FnParamNameExpected { token: Token },

    #[error("Function parameter type expected: {token}")]
    FnParamTypeExpected { token: Token },

    #[error("Function parameter comma expected as a delimiter: {token}")]
    FnParamCommaExpected { token: Token },

    #[error("Function parameter incomplete: {token}")]
    FnParameterIncomplete { token: Token },

    #[error("Closing parenthesis expected: {token}")]
    CloseParenExpected { token: Token },

    #[error("Function body expected: {token}")]
    FnBodyExpected { token: Token },

    #[error("Function return type expected after ':': {token}")]
    FnReturnTypeExpected { token: Token },

    #[error("Block expected close brace '}}': {token}")]
    BlockExpectedCloseBrace { token: Token },

    #[error("Expected identifier: {token}")]
    ExpectedIdentifier { token: Token },

    #[error("Let expression requires an initializer '=': {token}")]
    LetInitializerExpected { token: Token },

    #[error("Semicolon expected after an expression statement: {token}")]
    SemicolonExpectedAfterExpressionStatement { token: Token },

    #[error("Invalid left-hand side in assignment: {span}")]
    InvalidLHSInAssignment { span: DisplaySpan },

    #[error("Internal parser error: {0}")]
    InternalError(String),

    #[error("Expected node here: {span}")]
    ExpectedNode { span: DisplaySpan },

    #[error("Expression expected: {token}")]
    ExpressionExpected { token: Token },
}

impl Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?} at {:?}", self.kind, self.span) // More informative display
    }
}
