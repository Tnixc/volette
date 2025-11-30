use std::path::Path;

pub mod analysis;
pub mod codegen;
// #[macro_use]
// pub mod diagnostic_macros;
pub mod error;
pub mod lexer;
#[macro_use]
pub mod parser;
pub mod tokens;

use analysis::analyze;
use colored::Colorize;
use error::{ReportCollection, ResultWithDiagnostics, print_reports};
use lexer::Lexer;
use parser::Parser;
use string_interner::{StringInterner, backend::BucketBackend, symbol::SymbolUsize};
use tokens::{Span, Token, TokenKind};

use crate::compiler::parser::node::VType;

pub fn build(file: &Path) {
    let mut all_diagnostics = ReportCollection::new();

    let contents = match std::fs::read_to_string(file) {
        Ok(contents) => contents,
        Err(e) => {
            all_diagnostics.push(crate::lex_err!("Failed to read file: {}", None, e).into_cloneable());
            print_reports(&all_diagnostics);
            return;
        }
    };

    let mut interner: StringInterner<BucketBackend<SymbolUsize>> = StringInterner::new();
    let file_name = interner.get_or_intern(file.to_str().unwrap_or("<unknown>"));

    // ------------------------------ Lexing phase
    println!("{}", "=== Lexing Phase ===".bright_blue());
    let tokens = match lex_phase(&contents, &mut interner, file_name) {
        Ok(tokens) => tokens,
        Err(diagnostics) => {
            all_diagnostics.extend(diagnostics);
            print_reports(&all_diagnostics);
            return;
        }
    };

    // println!("Tokens: {:?}", format_tokens(&tokens, &interner));

    // ------------------------------ Parsing phase
    println!("{}", "=== Parsing Phase ===".bright_blue());
    let parse_result = match parse_phase(tokens, &mut interner) {
        Ok(result) => result,
        Err(diagnostics) => {
            all_diagnostics.extend(diagnostics);
            print_reports(&all_diagnostics);
            return;
        }
    };

    all_diagnostics.extend(parse_result.diagnostics);
    let (mut root, mut tree) = (parse_result.value.0, parse_result.value.1);

    parser::desugar::desugar(&mut root, &mut tree, &interner);

    root.print_tree(&tree, &interner);

    // ------------------------------ Analysis phase
    println!("{}", "=== Analysis Phase ===".bright_blue());
    let analysis_result = match analysis_phase(&root, &interner, &mut tree) {
        Ok(result) => result,
        Err(diagnostics) => {
            all_diagnostics.extend(diagnostics);
            print_reports(&all_diagnostics);
            return;
        }
    };

    all_diagnostics.extend(analysis_result.diagnostics);

    // stop if we have any errors
    if !all_diagnostics.is_empty() {
        print_reports(&all_diagnostics);
        return;
    }

    // codegen phase
    println!("{}", "=== Code Generation Phase ===".bright_blue());
    root.print_tree(&tree, &interner);

    match codegen::codegen(&root, &tree, &interner) {
        Ok(diag) => {
            all_diagnostics.extend(diag);
        }
        Err(e) => {
            all_diagnostics.push(e.into_cloneable());
        }
    }

    print_reports(&all_diagnostics);
    if all_diagnostics.is_empty() {
        println!("{}", "Compilation successful!".green().bold());
    }
}

fn lex_phase(
    contents: &str,
    interner: &mut StringInterner<BucketBackend<SymbolUsize>>,
    file_name: SymbolUsize,
) -> Result<Vec<Token>, ReportCollection> {
    let mut lexer = Lexer::new(interner, file_name);
    lexer.tokenize(contents.chars().collect());

    // Add EOF token
    lexer.tokens.push(Token::new(
        TokenKind::Eof,
        Span::new(
            file_name,
            lexer.cursor.line,
            lexer.cursor.col.max(0),
            lexer.cursor.col.max(0),
            lexer.cursor.col.max(0),
        ),
    ));

    if !lexer.errors.is_empty() {
        return Err(lexer.errors);
    }

    Ok(lexer.tokens)
}

fn parse_phase(
    tokens: Vec<Token>,
    interner: &mut StringInterner<BucketBackend<SymbolUsize>>,
) -> Result<
    ResultWithDiagnostics<(
        crate::compiler::parser::node::Node,
        generational_arena::Arena<crate::compiler::parser::node::Node>,
    )>,
    ReportCollection,
> {
    let mut parser = Parser::new(tokens, interner);
    let root = parser.parse();

    let parse_errors = std::mem::take(&mut parser.parse_errors);
    let diagnostics: ReportCollection = parse_errors.into_iter().collect();

    if !diagnostics.is_empty() {
        return Err(diagnostics);
    }

    Ok(ResultWithDiagnostics::with_diagnostics((root, parser.tree), diagnostics))
}

fn analysis_phase(
    root: &crate::compiler::parser::node::Node,
    interner: &StringInterner<BucketBackend<SymbolUsize>>,
    nodes: &mut generational_arena::Arena<crate::compiler::parser::node::Node>,
) -> Result<ResultWithDiagnostics<std::collections::HashMap<SymbolUsize, (Vec<VType>, VType)>>, ReportCollection> {
    let analysis_result = analyze(root, interner, nodes);

    if analysis_result.has_errors() {
        Err(analysis_result.diagnostics)
    } else {
        Ok(analysis_result)
    }
}

fn _format_tokens(tokens: &[Token], interner: &StringInterner<BucketBackend<SymbolUsize>>) -> Vec<(String, usize, (usize, usize))> {
    tokens
        .iter()
        .map(|t| {
            (
                if let TokenKind::Identifier(identifier) = t.kind {
                    format!("Identifier({})", interner.resolve(identifier).unwrap_or("<unknown>"))
                } else {
                    format!("{:?}", t.kind)
                },
                t.span.start.0,
                (t.span.start.1, t.span.end.1),
            )
        })
        .collect::<Vec<_>>()
}
