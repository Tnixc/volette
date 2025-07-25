use std::path::Path;

pub mod analysis;
pub mod codegen;
pub mod lexer;
pub mod parser;
pub mod tokens;

use analysis::analysis_pass;
use lexer::Lexer;
use parser::Parser;
use string_interner::{StringInterner, backend::BucketBackend, symbol::SymbolUsize};
use tokens::{Span, Token, TokenKind};

type Interner = StringInterner<BucketBackend<SymbolUsize>>;

pub fn build(file: &Path) {
    let contents = std::fs::read_to_string(file).unwrap_or_else(|_| panic!("Failed to read file: {:?}", file));
    let mut interner = Interner::new();
    let file_name = interner.get_or_intern(file.to_str().unwrap_or("<unknown>"));
    let mut lexer = Lexer::new(interner, file_name);

    lexer.tokenize(contents.chars().collect());
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
    println!("{:?}", lexer.format_tokens());

    lexer.print_errors();

    let mut parser = Parser::new(lexer.tokens, lexer.interner);
    let root = parser.parse();

    analysis_pass(&root, &parser.interner, &mut parser.tree);
    println!("After analysis");
    root.print_tree(&parser.tree, &parser.interner);
    // if parser.parse_errors.is_empty() {
    //     eprintln!(
    //         "Codegen Errors: {:?}",
    //         codegen::codegen(&root, &parser.tree, &mut parser.interner)
    //     );
    // } else {
    //     eprintln!("Errors: {:?}", parser.parse_errors);
    // }
}
