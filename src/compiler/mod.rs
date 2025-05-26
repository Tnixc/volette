use std::path::Path;

pub mod lexer;
pub mod tokens;

use lexer::Lexer;
use string_interner::{backend::BucketBackend, symbol::SymbolUsize, StringInterner};
type Interner = StringInterner<BucketBackend<SymbolUsize>>;

pub fn build(file: &Path) {
    let contents = std::fs::read_to_string(file).unwrap();
    let mut interner = Interner::new();
    let mut lexer = Lexer::new(&contents, interner.get_or_intern(file.to_str().unwrap()));
    let mut i = 0;
    while i < contents.len() {
        lexer.next(contents.chars().nth(i).unwrap());
        i += 1;
    }
    println!("{:#?}", lexer);
}

pub fn run(file: &Path) {
    todo!()
}
