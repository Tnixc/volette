use std::fs;
use string_interner::StringInterner;
use volette::compiler::analysis::analysis_pass;
use volette::compiler::codegen;
use volette::compiler::lexer::Lexer;
use volette::compiler::parser::Parser;
use volette::compiler::tokens::{Span, Token, TokenKind};

fn compile_full_pipeline(source: &str) -> Result<String, String> {
    let mut interner = StringInterner::new();
    let file_name = interner.get_or_intern("test.vt");

    let mut lexer = Lexer::new(&mut interner, file_name);
    lexer.tokenize(source.chars().collect());

    if !lexer.errors.is_empty() {
        return Err(format!("Lexing failed: {:?}", lexer.errors));
    }

    lexer.tokens.push(Token::new(
        TokenKind::Eof,
        Span::new(file_name, lexer.cursor.line, lexer.cursor.col, lexer.cursor.col, lexer.cursor.col),
    ));

    let token_count = lexer.tokens.len();

    let (root, mut tree, parse_errors) = {
        let mut parser = Parser::new(lexer.tokens, &mut interner);
        let root = parser.parse();
        let parse_errors = parser.parse_errors.clone();
        (root, parser.tree, parse_errors)
    };

    if !parse_errors.is_empty() {
        return Err(format!("Parsing failed: {:?}", parse_errors));
    }

    let analysis_result = analysis_pass(&root, &interner, &mut tree);

    if analysis_result.has_errors() {
        return Err(format!("Analysis failed: {:?}", analysis_result.diagnostics));
    }

    let fn_table = analysis_result.value;

    match codegen::codegen(&root, &tree, &interner, &fn_table) {
        Ok(diag) => {
            if diag.has_errors() {
                return Err(format!("Codegen failed: {:?}", diag));
            }
            Ok(format!("Full compilation successful: {} tokens processed", token_count))
        }
        Err(e) => Err(format!("Codegen error: {}", e)),
    }
}

#[test]
fn test_end_to_end_simple_arithmetic() {
    let source = fs::read_to_string("tests/fixtures/valid/simple_arithmetic.vt").unwrap();
    let result = compile_full_pipeline(&source);
    assert!(result.is_ok(), "Compilation failed: {:?}", result);
    insta::assert_snapshot!(result.unwrap());
}

#[test]
fn test_end_to_end_unary_ops() {
    let source = fs::read_to_string("tests/fixtures/valid/unary_ops.vt").unwrap();
    let result = compile_full_pipeline(&source);
    assert!(result.is_ok(), "Compilation failed: {:?}", result);
    insta::assert_snapshot!(result.unwrap());
}

#[test]
fn test_end_to_end_block_expr() {
    let source = fs::read_to_string("tests/fixtures/valid/block_expr.vt").unwrap();
    let result = compile_full_pipeline(&source);
    assert!(result.is_ok(), "Compilation failed: {:?}", result);
    insta::assert_snapshot!(result.unwrap());
}

#[test]
fn test_end_to_end_function_calls() {
    let source = fs::read_to_string("tests/fixtures/valid/function_calls.vt").unwrap();
    let result = compile_full_pipeline(&source);
    assert!(result.is_ok(), "Compilation failed: {:?}", result);
    insta::assert_snapshot!(result.unwrap());
}

#[test]
fn test_end_to_end_main() {
    let source = fs::read_to_string("tests/fixtures/valid/main.vt").unwrap();
    let result = compile_full_pipeline(&source);
    assert!(result.is_ok(), "Compilation failed: {:?}", result);
    insta::assert_snapshot!(result.unwrap());
}

#[test]
fn test_end_to_end_undefined_variable_error() {
    let source = fs::read_to_string("tests/fixtures/invalid/undefined_variable.vt").unwrap();
    let result = compile_full_pipeline(&source);
    assert!(result.is_err(), "Expected compilation to fail");
    insta::assert_snapshot!(result.unwrap_err());
}

#[test]
fn test_end_to_end_type_mismatch_error() {
    let source = fs::read_to_string("tests/fixtures/invalid/type_mismatch.vt").unwrap();
    let result = compile_full_pipeline(&source);
    assert!(result.is_err(), "Expected compilation to fail");
    insta::assert_snapshot!(result.unwrap_err());
}

#[test]
fn test_end_to_end_undefined_function_error() {
    let source = fs::read_to_string("tests/fixtures/invalid/undefined_function.vt").unwrap();
    let result = compile_full_pipeline(&source);
    assert!(result.is_err(), "Expected compilation to fail");
    insta::assert_snapshot!(result.unwrap_err());
}

#[test]
fn test_pipeline_multiple_functions() {
    let source = r#"
fn add(a: i32, b: i32): i32 {
    return a + b;
}

fn sub(a: i32, b: i32): i32 {
    return a - b;
}

fn compute(x: i32, y: i32): i32 {
    let sum = add(x, y);
    let diff = sub(x, y);
    return sum + diff;
}
"#;
    let result = compile_full_pipeline(source);
    assert!(result.is_ok(), "Compilation failed: {:?}", result);
    insta::assert_snapshot!(result.unwrap());
}

#[test]
fn test_pipeline_complex_expression() {
    let source = r#"
fn factorial_helper(n: i32, acc: i32): i32 {
    return n * acc;
}

fn test(x: i32): i32 {
    return factorial_helper(x, x - 1);
}
"#;
    let result = compile_full_pipeline(source);
    assert!(result.is_ok(), "Compilation failed: {:?}", result);
    insta::assert_snapshot!(result.unwrap());
}
