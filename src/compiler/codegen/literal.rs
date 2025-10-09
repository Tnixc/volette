use cranelift::prelude::{FunctionBuilder, InstBuilder, Value, types};

use crate::compiler::{
    codegen::error::TranslateError,
    parser::node::{Literal, Node, Type},
    tokens::PrimitiveTypes,
};

pub fn match_literal(
    literal: Literal,
    type_: Type,
    node: &Node,
    fn_builder: &mut FunctionBuilder,
) -> Result<Option<Value>, TranslateError> {
    match literal {
        Literal::Bool(v) => {
            if type_ != Type::Primitive(PrimitiveTypes::Bool) {
                return Err(TranslateError::IncorrectTypeAnalysis { type_, node: node.clone() });
            }
            Ok(Some(if v {
                fn_builder.ins().iconst(types::I8, 1)
            } else {
                fn_builder.ins().iconst(types::I8, 0)
            }))
        }
        Literal::Float(f) => match type_ {
            Type::Primitive(PrimitiveTypes::F64) => Ok(Some(fn_builder.ins().f64const(f))),
            Type::Primitive(PrimitiveTypes::F32) => Ok(Some(fn_builder.ins().f32const(f as f32))),
            _ => Err(TranslateError::IncorrectTypeAnalysis { type_, node: node.clone() }),
        },
        Literal::Int(i) => match type_ {
            Type::Primitive(PrimitiveTypes::I64) | Type::Primitive(PrimitiveTypes::U64) => Ok(Some(fn_builder.ins().iconst(types::I64, i))),
            Type::Primitive(PrimitiveTypes::I32) | Type::Primitive(PrimitiveTypes::U32) => Ok(Some(fn_builder.ins().iconst(types::I32, i))),
            Type::Primitive(PrimitiveTypes::I16) | Type::Primitive(PrimitiveTypes::U16) => Ok(Some(fn_builder.ins().iconst(types::I16, i))),
            Type::Primitive(PrimitiveTypes::I8) | Type::Primitive(PrimitiveTypes::U8) => Ok(Some(fn_builder.ins().iconst(types::I8, i))),
            Type::Primitive(PrimitiveTypes::Isize) | Type::Primitive(PrimitiveTypes::Usize) => {
                // TODO: make this respect system ptr width
                Ok(Some(fn_builder.ins().iconst(types::I64, i)))
            }
            _ => Err(TranslateError::IncorrectTypeAnalysis { type_, node: node.clone() }),
        },
        Literal::Nil => {
            if type_ != Type::Primitive(PrimitiveTypes::Nil) {
                return Err(TranslateError::IncorrectTypeAnalysis { type_, node: node.clone() });
            }
            Ok(None)
        }
    }
}
