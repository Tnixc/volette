use std::fs;
use string_interner::StringInterner;
use volette::compiler::lexer::Lexer;
use volette::compiler::parser::Parser;
use volette::compiler::tokens::{Span, Token, TokenKind};

fn parse_source(source: &str) -> String {
    let mut interner = StringInterner::new();
    let file_name = interner.get_or_intern("test.vt");

    let mut lexer = Lexer::new(&mut interner, file_name);
    lexer.tokenize(source.chars().collect());

    lexer.tokens.push(Token::new(
        TokenKind::Eof,
        Span::new(file_name, lexer.cursor.line, lexer.cursor.col, lexer.cursor.col, lexer.cursor.col),
    ));

    let mut parser = Parser::new(lexer.tokens, &mut interner);
    let root = parser.parse();

    root.to_json_like_string(&parser.tree, &interner)
}

#[test]
fn test_parse_simple_arithmetic() {
    let source = fs::read_to_string("tests/fixtures/valid/simple_arithmetic.vt").unwrap();
    let output = parse_source(&source);
    insta::assert_snapshot!(output);
}

#[test]
fn test_parse_unary_ops() {
    let source = fs::read_to_string("tests/fixtures/valid/unary_ops.vt").unwrap();
    let output = parse_source(&source);
    insta::assert_snapshot!(output);
}

#[test]
fn test_parse_block_expr() {
    let source = fs::read_to_string("tests/fixtures/valid/block_expr.vt").unwrap();
    let output = parse_source(&source);
    insta::assert_snapshot!(output);
}

#[test]
fn test_parse_function_calls() {
    let source = fs::read_to_string("tests/fixtures/valid/function_calls.vt").unwrap();
    let output = parse_source(&source);
    insta::assert_snapshot!(output);
}

#[test]
fn test_parse_main() {
    let source = fs::read_to_string("tests/fixtures/valid/main.vt").unwrap();
    let output = parse_source(&source);
    insta::assert_snapshot!(output);
}

#[test]
fn test_parse_single_function() {
    let source = r#"
fn test(x: i32): i32 {
    return x + 1;
}
"#;
    let output = parse_source(source);
    insta::assert_snapshot!(output);
}

#[test]
fn test_parse_nested_blocks() {
    let source = r#"
fn nested(): i32 {
    let x = {
        let a = 5;
        {
            let b = 10;
            a + b
        }
    }
    return x;
}
"#;
    let output = parse_source(source);
    insta::assert_snapshot!(output);
}

#[test]
fn test_parse_binary_ops() {
    let source = r#"
fn ops(a: i32, b: i32): i32 {
    let sum = a + b;
    let diff = a - b;
    let prod = a * b;
    let quot = a / b;
    return sum + diff + prod + quot;
}
"#;
    let output = parse_source(source);
    insta::assert_snapshot!(output);
}

#[test]
fn test_parse_pointer_arithmetic() {
    let source = r#"
fn ptr_arith(x: i32): i32 {
    let p = &x;
    let offset: usize = 4;
    let p2 = p + offset;
    return @p2;
}
"#;
    let output = parse_source(source);
    insta::assert_snapshot!(output);
}
