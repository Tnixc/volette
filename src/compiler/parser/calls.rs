use generational_arena::Index;
use string_interner::symbol::SymbolUsize;

use super::{error::ParserError, Parser};

impl Parser {
    pub fn parse_call(&mut self, name: SymbolUsize) -> Result<Index, ParserError> {
        todo!()
    }

    pub fn parse_method_call(&mut self, name: SymbolUsize) -> Result<Index, ParserError> {
        todo!()
    }
}
