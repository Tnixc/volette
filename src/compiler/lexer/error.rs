use std::fmt::{self, Display};

use thiserror::Error;

use crate::compiler::tokens::Span;

use super::numbers::NumberBase;

#[derive(Error, Debug, Clone)]
#[allow(dead_code)]
pub enum LexError {
    #[error("Unexpected character '{character}' at {span}")]
    UnexpectedCharacter { character: char, span: Span },

    #[error("Unterminated string literal starting at {span}")]
    UnterminatedString { span: Span },

    #[error("Invalid float '{value}' at {span}")]
    InvalidFloat {
        value: String,
        span: Span,
        #[source]
        source: std::num::ParseFloatError,
    },

    #[error("Invalid integer '{value}' at {span}")]
    InvalidInteger {
        value: String,
        span: Span,
        #[source]
        source: std::num::ParseIntError,
    },

    #[error("Non-integer base '{base}' at {span}")]
    NonIntegerBase { base: NumberBase, span: Span },
}

impl Display for NumberBase {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            NumberBase::Decimal => write!(f, "decimal"),
            NumberBase::Hex => write!(f, "0x hexadecimal"),
            NumberBase::Binary => write!(f, "0b binary"),
            NumberBase::Octal => write!(f, "0o octal"),
        }
    }
}

impl Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:#?}:{}", self.file, self.line)
    }
}
