use std::fs;
use string_interner::StringInterner;
use volette::compiler::analysis::analysis_pass;
use volette::compiler::lexer::Lexer;
use volette::compiler::parser::Parser;
use volette::compiler::tokens::{Span, Token, TokenKind};

fn analyze_source(source: &str) -> Result<String, String> {
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
        let parse_errors = parser.parse_errors.clone();
        (root, parser.tree, parse_errors)
    };

    if !parse_errors.is_empty() {
        return Err(format!("Parse errors: {:?}", parse_errors));
    }

    let result = analysis_pass(&root, &interner, &mut tree);

    if result.has_errors() {
        let errors = result
            .diagnostics
            .diagnostics
            .iter()
            .map(|d| format!("{:?}: {}", d.severity, d.message))
            .collect::<Vec<_>>()
            .join("\n");
        return Err(errors);
    }

    Ok(root.to_json_like_string(&tree, &interner))
}

#[test]
fn test_analyze_simple_arithmetic() {
    let source = fs::read_to_string("tests/fixtures/valid/simple_arithmetic.vt").unwrap();
    let output = analyze_source(&source).unwrap();
    insta::assert_snapshot!(output);
}

#[test]
fn test_analyze_unary_ops() {
    let source = fs::read_to_string("tests/fixtures/valid/unary_ops.vt").unwrap();
    let output = analyze_source(&source).unwrap();
    insta::assert_snapshot!(output);
}

#[test]
fn test_analyze_block_expr() {
    let source = fs::read_to_string("tests/fixtures/valid/block_expr.vt").unwrap();
    let output = analyze_source(&source).unwrap();
    insta::assert_snapshot!(output);
}

#[test]
fn test_analyze_function_calls() {
    let source = fs::read_to_string("tests/fixtures/valid/function_calls.vt").unwrap();
    let output = analyze_source(&source).unwrap();
    insta::assert_snapshot!(output);
}

#[test]
fn test_analyze_main() {
    let source = fs::read_to_string("tests/fixtures/valid/main.vt").unwrap();
    let output = analyze_source(&source).unwrap();
    insta::assert_snapshot!(output);
}

#[test]
fn test_analyze_undefined_variable() {
    let source = fs::read_to_string("tests/fixtures/invalid/undefined_variable.vt").unwrap();
    let result = analyze_source(&source);
    assert!(result.is_err());
    insta::assert_snapshot!(result.unwrap_err());
}

#[test]
fn test_analyze_undefined_function() {
    let source = fs::read_to_string("tests/fixtures/invalid/undefined_function.vt").unwrap();
    let result = analyze_source(&source);
    assert!(result.is_err());
    insta::assert_snapshot!(result.unwrap_err());
}

#[test]
fn test_analyze_pointer_types() {
    let source = r#"
fn test(x: i32): i32 {
    let p = &x;
    let val = @p;
    return val;
}
"#;
    let output = analyze_source(source).unwrap();
    insta::assert_snapshot!(output);
}

#[test]
fn test_analyze_type_inference() {
    let source = r#"
fn test(): i32 {
    let x = 10;
    let y = 20;
    let z = x + y;
    return z;
}
"#;
    let output = analyze_source(source).unwrap();
    insta::assert_snapshot!(output);
}
