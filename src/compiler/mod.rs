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

    let mut window_buffer = String::with_capacity(10);
    let chars: Vec<char> = contents.chars().chain(std::iter::once(' ')).collect();
    for (i, &ch) in chars.iter().enumerate() {
        window_buffer.clear();
        let window_end = (i + 10).min(chars.len());

        for &window_char in &chars[i..window_end] {
            window_buffer.push(window_char);
        }

        lexer.next(ch, &window_buffer);
    }
    println!("{:#?}", lexer);
}

pub fn run(file: &Path) {
    todo!()
}
