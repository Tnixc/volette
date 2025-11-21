use std::fs;
use string_interner::StringInterner;
use volette::compiler::analysis::analysis_pass;
use volette::compiler::codegen;
use volette::compiler::lexer::Lexer;
use volette::compiler::parser::Parser;
use volette::compiler::tokens::{Span, Token, TokenKind};

fn codegen_source(source: &str) -> Result<String, String> {
    let mut interner = StringInterner::new();
    let file_name = interner.get_or_intern("test.vt");

    let mut lexer = Lexer::new(&mut interner, file_name);
    lexer.tokenize(source.chars().collect());

    if !lexer.errors.is_empty() {
        return Err(format!("Lexer errors: {:?}", lexer.errors));
    }

    lexer.tokens.push(Token::new(
        TokenKind::Eof,
        Span::new(file_name, lexer.cursor.line, lexer.cursor.col, lexer.cursor.col, lexer.cursor.col),
    ));

    let (root, mut tree, parse_errors) = {
        let mut parser = Parser::new(lexer.tokens, &mut interner);
        let root = parser.parse();
        let parse_errors = std::mem::take(&mut parser.parse_errors);
        (root, parser.tree, parse_errors)
    };

    if !parse_errors.is_empty() {
        return Err(format!("Parse errors: {:?}", parse_errors));
    }

    let analysis_result = analysis_pass(&root, &interner, &mut tree);

    if analysis_result.has_errors() {
        return Err(format!("{:?}", analysis_result.diagnostics));
    }

    let fn_table = analysis_result.value;

    match codegen::codegen(&root, &tree, &interner, &fn_table) {
        Ok(diag) => {
            if !diag.is_empty() {
                return Err(format!("{:?}", diag));
            }
            Ok("Codegen successful".to_string())
        }
        Err(e) => Err(format!("Codegen error: {}", e)),
    }
}

#[test]
fn test_codegen_simple_arithmetic() {
    let source = fs::read_to_string("tests/fixtures/valid/simple_arithmetic.vt").unwrap();
    let result = codegen_source(&source);
    assert!(result.is_ok(), "Expected success, got: {:?}", result);
    insta::assert_snapshot!(result.unwrap());
}

#[test]
fn test_codegen_unary_ops() {
    let source = fs::read_to_string("tests/fixtures/valid/unary_ops.vt").unwrap();
    let result = codegen_source(&source);
    assert!(result.is_ok(), "Expected success, got: {:?}", result);
    insta::assert_snapshot!(result.unwrap());
}

#[test]
fn test_codegen_block_expr() {
    let source = fs::read_to_string("tests/fixtures/valid/block_expr.vt").unwrap();
    let result = codegen_source(&source);
    assert!(result.is_ok(), "Expected success, got: {:?}", result);
    insta::assert_snapshot!(result.unwrap());
}

#[test]
fn test_codegen_function_calls() {
    let source = fs::read_to_string("tests/fixtures/valid/function_calls.vt").unwrap();
    let result = codegen_source(&source);
    assert!(result.is_ok(), "Expected success, got: {:?}", result);
    insta::assert_snapshot!(result.unwrap());
}

#[test]
fn test_codegen_main() {
    let source = fs::read_to_string("tests/fixtures/valid/main.vt").unwrap();
    let result = codegen_source(&source);
    assert!(result.is_ok(), "Expected success, got: {:?}", result);
    insta::assert_snapshot!(result.unwrap());
}

#[test]
fn test_codegen_negation() {
    let source = r#"
fn negate(x: i32): i32 {
    return -x;
}
"#;
    let result = codegen_source(source);
    assert!(result.is_ok(), "Expected success, got: {:?}", result);
    insta::assert_snapshot!(result.unwrap());
}

#[test]
fn test_codegen_pointer_ops() {
    let source = r#"
fn ptr_test(x: i32): i32 {
    let p = &x;
    return @p;
}
"#;
    let result = codegen_source(source);
    assert!(result.is_ok(), "Expected success, got: {:?}", result);
    insta::assert_snapshot!(result.unwrap());
}

#[test]
fn test_codegen_complex_expression() {
    let source = r#"
fn complex(a: i32, b: i32): i32 {
    return (a + b) * (a - b);
}
"#;
    let result = codegen_source(source);
    assert!(result.is_ok(), "Expected success, got: {:?}", result);
    insta::assert_snapshot!(result.unwrap());
}
