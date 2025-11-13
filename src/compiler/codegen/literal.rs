use cranelift::prelude::{FunctionBuilder, InstBuilder, Value, types};
use string_interner::{StringInterner, backend::BucketBackend, symbol::SymbolUsize};

use crate::compiler::{
    codegen::error::TranslateError,
    parser::node::{Literal, Node, VType},
    tokens::PrimitiveTypes,
};

pub fn match_literal(
    literal: Literal,
    type_: VType,
    node: &Node,
    fn_builder: &mut FunctionBuilder,
    interner: &StringInterner<BucketBackend<SymbolUsize>>,
) -> Result<Option<Value>, TranslateError> {
    match literal {
        Literal::Bool(v) => {
            if type_ != VType::Primitive(PrimitiveTypes::Bool) {
                return Err(TranslateError::TypeError {
                    message: format!("expected bool literal but got type {:?}", type_),
                    span: node.span.to_display(interner),
                });
            }
            Ok(Some(if v {
                fn_builder.ins().iconst(types::I8, 1)
            } else {
                fn_builder.ins().iconst(types::I8, 0)
            }))
        }
        Literal::Float(f) => match type_ {
            VType::Primitive(PrimitiveTypes::F64) => Ok(Some(fn_builder.ins().f64const(f))),
            VType::Primitive(PrimitiveTypes::F32) => Ok(Some(fn_builder.ins().f32const(f as f32))),
            _ => Err(TranslateError::TypeError {
                message: format!("expected f32 or f64 for float literal but got type {:?}", type_),
                span: node.span.to_display(interner),
            }),
        },
        Literal::Int(i) => match type_ {
            VType::Primitive(PrimitiveTypes::I64 | PrimitiveTypes::U64) => Ok(Some(fn_builder.ins().iconst(types::I64, i))),
            VType::Primitive(PrimitiveTypes::I32 | PrimitiveTypes::U32) => Ok(Some(fn_builder.ins().iconst(types::I32, i))),
            VType::Primitive(PrimitiveTypes::I16 | PrimitiveTypes::U16) => Ok(Some(fn_builder.ins().iconst(types::I16, i))),
            VType::Primitive(PrimitiveTypes::I8 | PrimitiveTypes::U8) => Ok(Some(fn_builder.ins().iconst(types::I8, i))),
            VType::Primitive(PrimitiveTypes::Isize | PrimitiveTypes::Usize) => {
                // TODO: make this respect system ptr width
                Ok(Some(fn_builder.ins().iconst(types::I64, i)))
            }
            _ => Err(TranslateError::TypeError {
                message: format!("expected integer type for int literal but got type {:?}", type_),
                span: node.span.to_display(interner),
            }),
        },
        Literal::Nil => {
            if type_ != VType::Primitive(PrimitiveTypes::Nil) {
                return Err(TranslateError::TypeError {
                    message: format!("expected nil literal but got type {:?}", type_),
                    span: node.span.to_display(interner),
                });
            }
            Ok(None)
        }
    }
}
