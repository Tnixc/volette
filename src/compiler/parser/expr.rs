use generational_arena::Index;
use string_interner::symbol::SymbolUsize;

use super::{error::ParserError, Parser};

impl Parser {
    pub fn parse_expr(&mut self) -> Result<Index, ParserError> {
        todo!()
    }
}
