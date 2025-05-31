use std::path::Path;

pub mod lexer;
pub mod parser;
pub mod tokens;

use lexer::Lexer;
use parser::Parser;
use string_interner::{backend::BucketBackend, symbol::SymbolUsize, StringInterner};
use tokens::{Span, Token, TokenKind};

type Interner = StringInterner<BucketBackend<SymbolUsize>>;

pub fn build(file: &Path) {
    let contents = std::fs::read_to_string(file).unwrap_or_else(|_| panic!("Failed to read file: {:?}", file));
    let mut interner = Interner::new();
    let file_name = interner.get_or_intern(file.to_str().unwrap_or("<unknown>"));
    let mut lexer = Lexer::new(&contents, interner, file_name);

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
    parser.parse();
}
