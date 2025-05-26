use super::{error::LexError, Lexer, NumberBase};

impl Lexer {
    pub fn parse_float(&mut self) -> Result<f64, ()> {
        self.current_str.parse::<f64>().map_err(|e| {
            self.errors.push(LexError::InvalidFloat {
                value: self.current_str.clone(),
                span: self.span.clone(),
                source: e,
            });
        })
    }

    pub fn parse_int_with_base(&mut self, base: NumberBase) -> Result<i64, ()> {
        let radix = match base {
            NumberBase::Decimal => 10,
            NumberBase::Hex => 16,
            NumberBase::Binary => 2,
            NumberBase::Octal => 8,
        };

        let result = if base == NumberBase::Decimal {
            self.current_str.parse::<i64>()
        } else {
            i64::from_str_radix(&self.current_str, radix)
        };

        result.map_err(|e| {
            self.errors.push(LexError::InvalidInteger {
                value: self.current_str.clone(),
                span: self.span.clone(),
                source: e,
            });
        })
    }
}
