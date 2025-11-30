use colored::Colorize;
use derive_more::Display;
use rootcause::handlers::AttachmentHandler;
use rootcause::prelude::*;
pub use rootcause::report_collection::ReportCollection;
use std::fmt;

use crate::compiler::tokens::DisplaySpan;

/// Represents the phase of compilation where an error occurred
#[derive(Debug, Clone, Copy, PartialEq, Eq, Display)]
pub enum CompilerPhase {
    #[display("Lexing")]
    Lexing,
    #[display("Parsing")]
    Parsing,
    #[display("Analysis")]
    Analysis,
    #[display("Validation")]
    Validation,
    #[display("Code Generation")]
    Codegen,
}

/// Severity level of an error or diagnostic
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Display)]
pub enum Severity {
    #[display("{}", "hint".dimmed())]
    Hint,
    #[display("{}", "info".blue())]
    Info,
    #[display("{}", "warning".yellow())]
    Warning,
    #[display("{}", "error".red())]
    Error,
    #[display("{}", "fatal".red().bold())]
    Fatal,
}

#[derive(Debug, Clone, Display)]
#[display("{} {}", "help:".green().bold(), _0)]
pub struct Help(pub String);

#[derive(Debug, Clone, Display)]
#[display("{} {}", "note:".blue().bold(), _0)]
pub struct Note(pub String);

#[derive(Debug, Display)]
#[display("{}", _0)]
pub struct SimpleError(pub String);

impl std::error::Error for SimpleError {}

pub type CompilerResult<T> = Result<T, Report>;

pub struct ResultWithDiagnostics<T> {
    pub value: T,
    pub diagnostics: ReportCollection,
}

impl<T> ResultWithDiagnostics<T> {
    pub fn new(value: T) -> Self {
        Self {
            value,
            diagnostics: ReportCollection::new(),
        }
    }

    pub fn with_diagnostics(value: T, diagnostics: ReportCollection) -> Self {
        Self { value, diagnostics }
    }

    pub fn push_error(&mut self, error: Report) {
        self.diagnostics.push(error.into_cloneable());
    }

    pub fn extend_diagnostics(&mut self, other: ReportCollection) {
        for report in other {
            self.diagnostics.push(report);
        }
    }

    pub fn has_errors(&self) -> bool {
        !self.diagnostics.is_empty()
    }
}

struct SourceSnippetHandler;

impl AttachmentHandler<DisplaySpan> for SourceSnippetHandler {
    fn display(span: &DisplaySpan, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{}", span)?;

        if let Ok(content) = std::fs::read_to_string(&span.file) {
            let lines: Vec<&str> = content.lines().collect();
            let line_idx = span.start.0.saturating_sub(1);

            if let Some(line) = lines.get(line_idx) {
                let line_num = span.start.0.to_string();
                let gutter_width = line_num.len();
                let padding = " ".repeat(gutter_width);

                // writeln!(f, "{} {} {}", padding, "|".blue(), "".blue())?;
                writeln!(f, "{} {} {}", line_num.blue(), "🮌".blue(), line)?;

                let col_start = span.start.1.saturating_sub(1);
                let indicator_padding = " ".repeat(col_start);

                let len = if span.end.0 == span.start.0 {
                    span.end.1.saturating_sub(span.start.1).saturating_add(1).max(1)
                } else {
                    line.len().saturating_sub(col_start).max(1)
                };

                let carets = "▔".repeat(len);
                write!(f, "{} {} {}{}", padding, "🮌".blue(), indicator_padding, carets.red().bold())?;
            }
        }
        Ok(())
    }

    fn debug(span: &DisplaySpan, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Self::display(span, f)
    }
}

#[macro_export]
macro_rules! lex_err {
    ($msg:expr, $span:expr) => {
        $crate::compiler::error::create_report(
            $crate::compiler::error::Severity::Error,
            $msg,
            $crate::compiler::error::CompilerPhase::Lexing,
            $span,
        )
    };
    ($msg:expr, $span:expr, $($arg:tt)*) => {
        $crate::compiler::error::create_report(
            $crate::compiler::error::Severity::Error,
            format!($msg, $($arg)*),
            $crate::compiler::error::CompilerPhase::Lexing,
            $span,
        )
    };
}

#[macro_export]
macro_rules! parse_err {
    ($msg:expr, $span:expr) => {
        $crate::compiler::error::create_report(
            $crate::compiler::error::Severity::Error,
            $msg,
            $crate::compiler::error::CompilerPhase::Parsing,
            $span,
        )
    };
    ($msg:expr, $span:expr, $($arg:tt)*) => {
        $crate::compiler::error::create_report(
            $crate::compiler::error::Severity::Error,
            format!($msg, $($arg)*),
            $crate::compiler::error::CompilerPhase::Parsing,
            $span,
        )
    };
}

#[macro_export]
macro_rules! analysis_err {
    ($msg:expr, $span:expr) => {
        $crate::compiler::error::create_report(
            $crate::compiler::error::Severity::Error,
            $msg,
            $crate::compiler::error::CompilerPhase::Analysis,
            $span,
        )
    };
    ($msg:expr, $span:expr, $($arg:tt)*) => {
        $crate::compiler::error::create_report(
            $crate::compiler::error::Severity::Error,
            format!($msg, $($arg)*),
            $crate::compiler::error::CompilerPhase::Analysis,
            $span,
        )
    };
}

#[macro_export]
macro_rules! codegen_err {
    ($msg:expr, $span:expr) => {
        $crate::compiler::error::create_report(
            $crate::compiler::error::Severity::Error,
            $msg,
            $crate::compiler::error::CompilerPhase::Codegen,
            $span,
        )
    };
    ($msg:expr, $span:expr, $($arg:tt)*) => {
        $crate::compiler::error::create_report(
            $crate::compiler::error::Severity::Error,
            format!($msg, $($arg)*),
            $crate::compiler::error::CompilerPhase::Codegen,
            $span,
        )
    };
}

pub fn create_report(severity: Severity, message: impl Into<String>, phase: CompilerPhase, span: Option<DisplaySpan>) -> Report {
    let phase_str = format!("[{}]", phase).dimmed();
    let msg_str = message.into().bright_white().bold();
    let msg = format!("{}{}: {}", severity, phase_str, msg_str);

    let mut report = Report::new(SimpleError(msg));

    if let Some(s) = span {
        report = report.attach_custom::<SourceSnippetHandler, _>(s);
    }
    report.into()
}

pub fn print_reports(reports: &ReportCollection) {
    for report in reports {
        // we want to print the report but hide the location of creation
        // rootcause doesn't seem to have an easy way to disable location capture.
        // so format it to a string and filter out the line.
        // this is a bit hacky but effective for display purposes.
        // FIXME: when rootcause adds a way to exclude src
        let output = format!("{:?}", report);
        for line in output.lines() {
            if !line.contains("src/compiler/error.rs") && !line.contains("src/compiler/diagnostic_macros.rs") {
                eprintln!("{}", line);
            }
        }
    }
}
