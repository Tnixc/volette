use colored::Colorize;
use std::fmt::{self, Display};
use thiserror::Error;

use crate::compiler::{
    analysis::error::AnalysisError, codegen::error::TranslateError, lexer::error::LexError, parser::error::ParserError, tokens::DisplaySpan,
};

/// Represents the phase of compilation where an error occurred
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CompilerPhase {
    Lexing,
    Parsing,
    Analysis,
    Validation,
    Codegen,
}

impl Display for CompilerPhase {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CompilerPhase::Lexing => write!(f, "Lexing"),
            CompilerPhase::Parsing => write!(f, "Parsing"),
            CompilerPhase::Analysis => write!(f, "Analysis"),
            CompilerPhase::Validation => write!(f, "Validation"),
            CompilerPhase::Codegen => write!(f, "Code Generation"),
        }
    }
}

/// Severity level of an error or diagnostic
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Severity {
    Hint,
    Info,
    Warning,
    Error,
    Fatal,
}

impl Display for Severity {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Severity::Hint => write!(f, "{}", "hint".dimmed()),
            Severity::Info => write!(f, "{}", "info".blue()),
            Severity::Warning => write!(f, "{}", "warning".yellow()),
            Severity::Error => write!(f, "{}", "error".red()),
            Severity::Fatal => write!(f, "{}", "fatal".red().bold()),
        }
    }
}

/// A unified compiler error that can contain errors from any phase
#[derive(Debug, Error, Clone)]
pub enum CompilerError {
    #[error("Lexical error: {0}")]
    Lex(#[from] LexError),

    #[error("Parse error: {0}")]
    Parse(#[from] ParserError),

    #[error("Analysis error: {0}")]
    Analysis(#[from] AnalysisError),

    #[error("Validation error: {message}")]
    Validation { message: String, span: DisplaySpan },

    #[error("Code generation error: {0}")]
    Codegen(#[from] TranslateError),

    #[error("Internal compiler error: {message}")]
    Internal { message: String, span: Option<DisplaySpan> },
}

impl CompilerError {
    pub fn phase(&self) -> CompilerPhase {
        match self {
            CompilerError::Lex(_) => CompilerPhase::Lexing,
            CompilerError::Parse(_) => CompilerPhase::Parsing,
            CompilerError::Analysis(_) => CompilerPhase::Analysis,
            CompilerError::Validation { .. } => CompilerPhase::Validation,
            CompilerError::Codegen(_) => CompilerPhase::Codegen,
            CompilerError::Internal { .. } => CompilerPhase::Codegen, // Assume codegen for now
        }
    }

    pub fn severity(&self) -> Severity {
        match self {
            CompilerError::Lex(_) => Severity::Error,
            CompilerError::Parse(_) => Severity::Error,
            CompilerError::Analysis(_) => Severity::Error,
            CompilerError::Validation { .. } => Severity::Error,
            CompilerError::Codegen(_) => Severity::Error,
            CompilerError::Internal { .. } => Severity::Fatal,
        }
    }

    pub fn span(&self) -> Option<&DisplaySpan> {
        match self {
            CompilerError::Lex(e) => Some(e.span()),
            CompilerError::Parse(e) => e.span(),
            CompilerError::Analysis(e) => e.span(),
            CompilerError::Validation { span, .. } => Some(span),
            CompilerError::Codegen(e) => e.span(),
            CompilerError::Internal { span, .. } => span.as_ref(),
        }
    }

    pub fn validation(message: impl Into<String>, span: DisplaySpan) -> Self {
        CompilerError::Validation {
            message: message.into(),
            span,
        }
    }

    pub fn internal(message: impl Into<String>, span: Option<DisplaySpan>) -> Self {
        CompilerError::Internal {
            message: message.into(),
            span,
        }
    }
}

/// A diagnostic message that can be an error, warning, or info
#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub severity: Severity,
    pub message: String,
    pub span: Option<DisplaySpan>,
    pub phase: CompilerPhase,
    pub help: Option<String>,
    pub note: Option<String>,
}

impl Diagnostic {
    pub fn new(severity: Severity, message: impl Into<String>, phase: CompilerPhase) -> Self {
        Self {
            severity,
            message: message.into(),
            span: None,
            phase,
            help: None,
            note: None,
        }
    }

    pub fn error(message: impl Into<String>, phase: CompilerPhase) -> Self {
        Self::new(Severity::Error, message, phase)
    }

    pub fn warning(message: impl Into<String>, phase: CompilerPhase) -> Self {
        Self::new(Severity::Warning, message, phase)
    }

    pub fn with_span(mut self, span: DisplaySpan) -> Self {
        self.span = Some(span);
        self
    }

    pub fn with_help(mut self, help: impl Into<String>) -> Self {
        self.help = Some(help.into());
        self
    }

    pub fn with_note(mut self, note: impl Into<String>) -> Self {
        self.note = Some(note.into());
        self
    }
}

impl From<CompilerError> for Diagnostic {
    fn from(error: CompilerError) -> Self {
        let mut diagnostic = Diagnostic {
            severity: error.severity(),
            message: error.to_string(),
            span: error.span().cloned(),
            phase: error.phase(),
            help: None,
            note: None,
        };

        // Add helpful context for common errors
        match &error {
            CompilerError::Parse(ParserError::ExpectedIdentifier { .. }) => {
                diagnostic =
                    diagnostic.with_help("Identifiers must start with a letter or underscore, followed by letters, digits, or underscores");
            }
            CompilerError::Parse(ParserError::FnBodyExpected { .. }) => {
                diagnostic = diagnostic.with_help("Function definitions require a body enclosed in braces { }");
            }
            CompilerError::Analysis(AnalysisError::TypeMismatch { expected, got, .. }) => {
                diagnostic = diagnostic.with_help(format!(
                    "Consider converting the value to {} or changing the expected type to {}",
                    expected, got
                ));
            }
            CompilerError::Analysis(AnalysisError::UnresolvedIdentifier { name, .. }) => {
                diagnostic = diagnostic.with_help(format!("Make sure '{}' is defined before it's used", name));
                diagnostic = diagnostic.with_note("Variables must be declared with 'let' before they can be used");
            }
            _ => {}
        }

        diagnostic
    }
}

/// Collection of diagnostics from compilation
#[derive(Debug, Clone)]
pub struct DiagnosticCollection {
    pub diagnostics: Vec<Diagnostic>,
}

impl DiagnosticCollection {
    pub fn new() -> Self {
        Self { diagnostics: Vec::new() }
    }

    pub fn push(&mut self, diagnostic: Diagnostic) {
        self.diagnostics.push(diagnostic);
    }

    pub fn extend(&mut self, other: DiagnosticCollection) {
        self.diagnostics.extend(other.diagnostics);
    }

    pub fn add_error(&mut self, error: CompilerError) {
        self.push(error.into());
    }

    pub fn error_count(&self) -> usize {
        self.diagnostics
            .iter()
            .filter(|d| matches!(d.severity, Severity::Error | Severity::Fatal))
            .count()
    }

    pub fn warning_count(&self) -> usize {
        self.diagnostics.iter().filter(|d| d.severity == Severity::Warning).count()
    }

    pub fn has_errors(&self) -> bool {
        self.error_count() > 0
    }

    pub fn is_empty(&self) -> bool {
        self.diagnostics.is_empty()
    }

    /// Sort diagnostics by severity (highest first) and then by location
    pub fn sort(&mut self) {
        self.diagnostics.sort_by(|a, b| {
            use std::cmp::Ordering;

            // First sort by severity (highest first)
            match b.severity.cmp(&a.severity) {
                Ordering::Equal => {
                    // Then by span location if available
                    match (&a.span, &b.span) {
                        (Some(a_span), Some(b_span)) => {
                            // Sort by file, then line, then column
                            a_span
                                .file
                                .cmp(&b_span.file)
                                .then_with(|| a_span.start.0.cmp(&b_span.start.0))
                                .then_with(|| a_span.start.1.cmp(&b_span.start.1))
                        }
                        (Some(_), None) => Ordering::Less,
                        (None, Some(_)) => Ordering::Greater,
                        (None, None) => Ordering::Equal,
                    }
                }
                other => other,
            }
        });
    }

    /// Print all diagnostics to stderr with nice formatting
    pub fn print_all(&self) {
        if self.is_empty() {
            return;
        }

        let mut sorted = self.clone();
        sorted.sort();

        for diagnostic in &sorted.diagnostics {
            self.print_diagnostic(diagnostic);
        }

        // Print summary
        let error_count = self.error_count();
        let warning_count = self.warning_count();

        eprintln!("{}", "─".repeat(60).dimmed());

        if error_count > 0 {
            eprintln!(
                "{}: {} error{} found",
                "Compilation failed".red().bold(),
                error_count,
                if error_count == 1 { "" } else { "s" }
            );
        }

        if warning_count > 0 {
            eprintln!(
                "{}: {} warning{} emitted",
                "Note".yellow(),
                warning_count,
                if warning_count == 1 { "" } else { "s" }
            );
        }

        if error_count == 0 && warning_count == 0 {
            eprintln!("{}", "Compilation successful".green().bold());
        }
    }

    fn print_diagnostic(&self, diagnostic: &Diagnostic) {
        // header with severity and phase
        eprintln!(
            "{} [{}]: {}",
            diagnostic.severity,
            diagnostic.phase.to_string().dimmed(),
            diagnostic.message.bold()
        );

        // Location information
        if let Some(span) = &diagnostic.span {
            eprintln!(" {} {}:{}", "-->".blue(), span.file, span.start.0);

            // try to show source context
            if let Some(source_line) = self.read_source_line(span) {
                let line_number = span.start.0;
                let line_number_width = line_number.to_string().len();
                let padding = " ".repeat(line_number_width);

                // empty line for spacing
                eprintln!("{} {}", padding, "|".blue());

                // source line with line number
                eprintln!("{} {} {}", line_number.to_string().blue(), "|".blue(), source_line);

                // error indicator
                let start_col = span.start.1.saturating_sub(1);
                let end_col = span.end.1;
                let indicator_length = (end_col - span.start.1).max(1);
                let indicator_padding = " ".repeat(start_col);
                let indicators = "^".repeat(indicator_length);

                eprintln!("{} {} {}{}", padding, "|".blue(), indicator_padding, indicators.red().bold());
            }
        }

        if let Some(help) = &diagnostic.help {
            eprintln!(" {} {}", "help:".green().bold(), help);
        }

        if let Some(note) = &diagnostic.note {
            eprintln!(" {} {}", "note:".blue().bold(), note);
        }

        eprintln!(); // Empty line for spacing
    }

    fn read_source_line(&self, span: &DisplaySpan) -> Option<String> {
        std::fs::read_to_string(&span.file)
            .ok()?
            .lines()
            .nth(span.start.0.saturating_sub(1))
            .map(|s| s.to_string())
    }
}

impl Default for DiagnosticCollection {
    fn default() -> Self {
        Self::new()
    }
}

impl std::iter::FromIterator<Diagnostic> for DiagnosticCollection {
    fn from_iter<T: IntoIterator<Item = Diagnostic>>(iter: T) -> Self {
        Self {
            diagnostics: iter.into_iter().collect(),
        }
    }
}

impl std::iter::FromIterator<CompilerError> for DiagnosticCollection {
    fn from_iter<T: IntoIterator<Item = CompilerError>>(iter: T) -> Self {
        Self {
            diagnostics: iter.into_iter().map(|e| e.into()).collect(),
        }
    }
}

/// Represents the result of a compilation phase
pub type CompilerResult<T> = Result<T, DiagnosticCollection>;

/// Helper trait for converting single errors to diagnostic collections
pub trait IntoDiagnostics<T> {
    fn into_diagnostics(self) -> CompilerResult<T>;
}

impl<T, E> IntoDiagnostics<T> for Result<T, E>
where
    E: Into<CompilerError>,
{
    fn into_diagnostics(self) -> CompilerResult<T> {
        match self {
            Ok(value) => Ok(value),
            Err(error) => {
                let mut diagnostics = DiagnosticCollection::new();
                diagnostics.add_error(error.into());
                Err(diagnostics)
            }
        }
    }
}

/// Result type for operations that can accumulate diagnostics but still return a value
pub struct ResultWithDiagnostics<T> {
    pub value: T,
    pub diagnostics: DiagnosticCollection,
}

impl<T> ResultWithDiagnostics<T> {
    pub fn new(value: T) -> Self {
        Self {
            value,
            diagnostics: DiagnosticCollection::new(),
        }
    }

    pub fn with_diagnostics(value: T, diagnostics: DiagnosticCollection) -> Self {
        Self { value, diagnostics }
    }

    pub fn push_diagnostic(&mut self, diagnostic: Diagnostic) {
        self.diagnostics.push(diagnostic);
    }

    pub fn push_error(&mut self, error: CompilerError) {
        self.diagnostics.add_error(error);
    }

    pub fn extend_diagnostics(&mut self, other: DiagnosticCollection) {
        self.diagnostics.extend(other);
    }

    pub fn has_errors(&self) -> bool {
        self.diagnostics.has_errors()
    }
}
