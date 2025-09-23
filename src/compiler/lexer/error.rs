use colored::Colorize;
use std::fmt::{self, Display};
use std::fs;
use string_interner::StringInterner;
use string_interner::backend::BucketBackend;
use string_interner::symbol::SymbolUsize;
use thiserror::Error;

use crate::compiler::tokens::DisplaySpan;
use crate::compiler::tokens::Span;

use super::Lexer;

#[derive(Error, Debug, Clone)]
pub enum LexError {
    #[error("Invalid character '{character}'")]
    InvalidCharacter { character: char, span: DisplaySpan },

    #[error("Unexpected character '{character}'")]
    UnexpectedCharacter { character: char, span: DisplaySpan },

    #[error("Unterminated string literal starting")]
    UnterminatedString { span: DisplaySpan },

    #[error("Invalid float '{value}'")]
    InvalidFloat {
        value: String,
        span: DisplaySpan,
        #[source]
        source: std::num::ParseFloatError,
    },

    #[error("Invalid integer '{value}'")]
    InvalidInteger {
        value: String,
        span: DisplaySpan,
        #[source]
        source: std::num::ParseIntError,
    },

    #[error("Non-decimal float")]
    NonDecimalFloat { span: DisplaySpan },
}

impl Span {
    pub fn to_display(&self, interner: &StringInterner<BucketBackend<SymbolUsize>>) -> DisplaySpan {
        DisplaySpan {
            file: interner.resolve(self.file).unwrap_or("<unknown>").to_string(),
            start: self.start,
            end: self.end,
        }
    }
}

impl DisplaySpan {
    fn read_source_line(&self) -> Option<String> {
        fs::read_to_string(&self.file)
            .ok()?
            .lines()
            .nth(self.start.0.saturating_sub(1))
            .map(|s| s.to_string())
    }

    fn format_error_display(&self, info: &str) -> String {
        let source_line = self.read_source_line();

        let mut output = String::new();

        // Error header with location
        output.push_str(&format!(
            "{}: {} \n {} {}:{}:{}\n",
            "Error".red().bold(),
            info.bold(),
            "-->".blue(),
            self.file,
            self.start.0,
            self.start.1
        ));

        // Line number padding
        let line_num_width = self.start.0.to_string().len();
        let padding = " ".repeat(line_num_width);

        output.push_str(&format!("{} {}\n", padding, "|".blue()));

        if let Some(line_content) = source_line {
            // Source line with line number
            output.push_str(&format!(
                "{} {} {}\n",
                self.start.0.to_string().blue(),
                "|".blue().bold(),
                line_content
            ));

            // Error indicator line
            let indicator_padding = " ".repeat(self.start.1.saturating_sub(1));
            let indicator_length = (self.end.1 - self.start.1).max(1);
            let indicators = "^".repeat(indicator_length);

            output.push_str(&format!(
                "{} {} {}{} {}\n",
                padding,
                "|".blue(),
                indicator_padding,
                indicators.red().bold(),
                info.red().bold()
            ));
        } else {
            output.push_str(&format!("{} {} {}\n", padding, "|".blue(), info.red().bold()));
        }

        output.push_str(&format!("{} {}", padding, "|".blue()));

        output
    }
}

impl Display for DisplaySpan {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}:{}-{}", self.file, self.start.0, self.start.1, self.end.1)
    }
}
impl LexError {
    pub fn span(&self) -> &DisplaySpan {
        match self {
            LexError::InvalidCharacter { span, .. }
            | LexError::UnexpectedCharacter { span, .. }
            | LexError::UnterminatedString { span }
            | LexError::InvalidFloat { span, .. }
            | LexError::InvalidInteger { span, .. }
            | LexError::NonDecimalFloat { span } => span,
        }
    }
}

impl<'a> Lexer<'a> {
    pub fn print_errors(&self) {
        for error in &self.errors {
            eprintln!("{}", error.span().format_error_display(format!("{}", error).as_str()));
        }
        println!("{}", "-".repeat(50).dimmed());
        if !self.errors.is_empty() {
            println!("{}: {} found", "Lex errors".red().bold(), self.errors.len());
        } else {
            println!("{}: {} found", "Lex errors".green().bold(), self.errors.len());
        }
    }
}
