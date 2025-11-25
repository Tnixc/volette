use rootcause::Report;
use string_interner::{StringInterner, backend::BucketBackend, symbol::SymbolUsize};

use crate::compiler::{
    analysis::compatible::can_convert,
    error::Help,
    parser::node::{Literal, VType},
    tokens::{PrimitiveTypes, Span},
};

pub fn check_literal(
    span: Span,
    interner: &StringInterner<BucketBackend<SymbolUsize>>,
    expected: Option<&VType>,
    literal: Literal,
) -> Result<VType, Report> {
    let inferred = match literal {
        Literal::Int(_) => VType::Primitive(PrimitiveTypes::I32),
        Literal::Float(_) => VType::Primitive(PrimitiveTypes::F32),
        Literal::Bool(_) => VType::Primitive(PrimitiveTypes::Bool),
        Literal::Nil => VType::Primitive(PrimitiveTypes::Nil),
    };

    let target = expected.unwrap_or(&inferred);

    if can_convert(&inferred, target) || &inferred == target {
        Ok(target.clone())
    } else {
        Err(crate::analysis_err!(
            "Type mismatch: expected {:?}, got {:?}",
            Some(span.to_display(interner)),
            target,
            inferred
        )
        .attach(Help("Ensure the types match or add an explicit conversion".into())))
    }
}
