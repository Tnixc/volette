use std::path::Path;

pub mod lexer;
pub mod tokens;

use lexer::Lexer;
use string_interner::{backend::BucketBackend, symbol::SymbolUsize, StringInterner};
use tokens::TokenKind;
type Interner = StringInterner<BucketBackend<SymbolUsize>>;

pub fn build(file: &Path) {
    let contents = std::fs::read_to_string(file).unwrap();
    let mut interner = Interner::new();
    let file_name = interner.get_or_intern(file.to_str().unwrap());
    let mut lexer = Lexer::new(&contents, interner, file_name);

    let chars: Vec<char> = contents.chars().chain(std::iter::once('\0')).collect();
    lexer.tokenize(chars);

    println!(
        "{:?}",
        lexer
            .tokens
            .iter()
            .map(|t| {
                (
                    if let TokenKind::Identifier(identifier) = t.kind {
                        format!("{}", lexer.interner.resolve(identifier).unwrap())
                    } else {
                        format!("{:?}", t.kind)
                    },
                    t.span.line,
                    (t.span.start, t.span.end),
                )
            })
            .collect::<Vec<_>>()
    );
}

pub fn run(file: &Path) {
    todo!()
}
