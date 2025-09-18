use cranelift::prelude::{EntityRef, FunctionBuilder, InstBuilder, Value, Variable, types};
use cranelift::module::Module;
use generational_arena::Index;
use std::collections::HashMap;
use string_interner::symbol::SymbolUsize;

use crate::compiler::{
    analysis::literal_default_types,
    codegen::{Info, error::TranslateError},
    parser::node::{BinOpKind, ExprKind, Literal, Node, NodeKind, Type},
    tokens::PrimitiveTypes,
};

pub fn expr_to_val(
    node: Index,
    fn_builder: &mut FunctionBuilder,
    scopes: &mut Vec<HashMap<SymbolUsize, (Type, Variable)>>,
    info: &mut Info,
) -> Result<(Value, Type), TranslateError> {
    let node = info.nodes.get(node).expect("[Expr to val] Node index not found in arena");
    match &node.kind {
        NodeKind::Expr { kind, type_ } => {
            let value = match kind {
                ExprKind::Literal(literal) => match_literal(*literal, type_.unwrap_or(literal_default_types(*literal)), node, fn_builder)?,
                ExprKind::BinOp { left, right, op } => expr_binop(*left, *right, *op, fn_builder, scopes, info)?,
                ExprKind::Return { value: ret_val } => expr_return(*ret_val, fn_builder, scopes, info)?,
                ExprKind::Block { exprs } => {
                    let (val, _) = expr_block(exprs, fn_builder, scopes, info)?;
                    val
                }
                ExprKind::LetBinding { name, value, .. } => {
                    let (var_val, actual_type) = expr_to_val(*value, fn_builder, scopes, info)?;
                    let ty = actual_type.to_clif(info.build_config.ptr_width);

                    // Create a unique variable index across all scopes
                    let var_index = scopes.iter().map(|s| s.len()).sum::<usize>();
                    let var = Variable::new(var_index);
                    fn_builder.declare_var(var, ty);
                    fn_builder.def_var(var, var_val);

                    scopes.last_mut().unwrap().insert(*name, (actual_type, var));

                    // A let binding's value is nil
                    // TODO: implement nil
                    fn_builder.ins().iconst(types::I32, 0)
                }
                ExprKind::Identifier(sym) => {
                    let var = scopes.iter().rev().find_map(|scope| scope.get(sym)).expect("Identifier not found");
                    println!("Variable: {:?}", var);
                    fn_builder.use_var(var.1)
                }
                ExprKind::Call { func, args } => {
                    expr_call(*func, args, fn_builder, scopes, info)?
                }
                _ => {
                    println!("valBefore: {:?} ||| {:?}", kind, type_);

                    todo!()
                }
            };

            println!("val: {:?} ||| {:?}", kind, type_);
            Ok((value, type_.unwrap_or(Type::Primitive(PrimitiveTypes::Never))))
            // TODO: Make the blocks accept -> statements like returns and eval its type instead of defaulting to Never
        }
        _ => todo!(),
    }
}

fn expr_binop(
    left: Index,
    right: Index,
    op: BinOpKind,
    fn_builder: &mut FunctionBuilder,
    scopes: &mut Vec<HashMap<SymbolUsize, (Type, Variable)>>,
    info: &mut Info,
) -> Result<Value, TranslateError> {
    let (left_value, left_type) = expr_to_val(left, fn_builder, scopes, info)?;
    let (right_value, right_type) = expr_to_val(right, fn_builder, scopes, info)?;

    let val = match (left_type, right_type) {
        (Type::Primitive(PrimitiveTypes::F32), Type::Primitive(PrimitiveTypes::F32))
        | (Type::Primitive(PrimitiveTypes::F64), Type::Primitive(PrimitiveTypes::F64)) => {
            use cranelift::codegen::ir::condcodes::FloatCC;

            match op {
                BinOpKind::Add => fn_builder.ins().fadd(left_value, right_value),
                BinOpKind::Sub => fn_builder.ins().fsub(left_value, right_value),
                BinOpKind::Mul => fn_builder.ins().fmul(left_value, right_value),
                BinOpKind::Div => fn_builder.ins().fdiv(left_value, right_value),
                BinOpKind::Eq => fn_builder.ins().fcmp(FloatCC::Equal, left_value, right_value),
                BinOpKind::NotEq => fn_builder.ins().fcmp(FloatCC::NotEqual, left_value, right_value),
                BinOpKind::GreaterThan => fn_builder.ins().fcmp(FloatCC::GreaterThan, left_value, right_value),
                BinOpKind::GreaterThanOrEq => fn_builder.ins().fcmp(FloatCC::GreaterThanOrEqual, left_value, right_value),
                BinOpKind::LessThan => fn_builder.ins().fcmp(FloatCC::LessThan, left_value, right_value),
                BinOpKind::LessThanOrEq => fn_builder.ins().fcmp(FloatCC::LessThanOrEqual, left_value, right_value),
                _ => todo!(),
            }
        }
        (Type::Primitive(PrimitiveTypes::I32), Type::Primitive(PrimitiveTypes::I32))
        | (Type::Primitive(PrimitiveTypes::I64), Type::Primitive(PrimitiveTypes::I64)) => {
            use cranelift::codegen::ir::condcodes::IntCC;
            match op {
                BinOpKind::Add => fn_builder.ins().iadd(left_value, right_value),
                BinOpKind::Sub => fn_builder.ins().isub(left_value, right_value),
                BinOpKind::Mul => fn_builder.ins().imul(left_value, right_value),
                BinOpKind::Div => fn_builder.ins().sdiv(left_value, right_value),
                BinOpKind::Eq => fn_builder.ins().icmp(IntCC::Equal, left_value, right_value),
                BinOpKind::NotEq => fn_builder.ins().icmp(IntCC::NotEqual, left_value, right_value),
                BinOpKind::GreaterThan => fn_builder.ins().icmp(IntCC::SignedGreaterThan, left_value, right_value),
                BinOpKind::GreaterThanOrEq => fn_builder.ins().icmp(IntCC::SignedGreaterThanOrEqual, left_value, right_value),
                BinOpKind::LessThan => fn_builder.ins().icmp(IntCC::SignedLessThan, left_value, right_value),
                BinOpKind::LessThanOrEq => fn_builder.ins().icmp(IntCC::SignedLessThanOrEqual, left_value, right_value),
                BinOpKind::Mod => fn_builder.ins().srem(left_value, right_value),
                _ => todo!(),
            }
        }

        _ => todo!(),
    };

    Ok(val)
}

fn expr_return(
    ret_val: Option<Index>,
    fn_builder: &mut FunctionBuilder,
    scopes: &mut Vec<HashMap<SymbolUsize, (Type, Variable)>>,
    info: &mut Info,
) -> Result<Value, TranslateError> {
    if let Some(ret_val) = ret_val {
        let (value, _) = expr_to_val(ret_val, fn_builder, scopes, info)?;
        fn_builder.ins().return_(&[value]);
        Ok(value)
    } else {
        fn_builder.ins().return_(&[]);
        Ok(Value::from_u32(0)) // TODO: implement proper never type
    }
}

fn match_literal(literal: Literal, type_: Type, node: &Node, fn_builder: &mut FunctionBuilder) -> Result<Value, TranslateError> {
    match literal {
        Literal::Bool(v) => {
            if type_ != Type::Primitive(PrimitiveTypes::Bool) {
                return Err(TranslateError::IncorrectTypeAnalysis { type_, node: node.clone() });
            }
            if v {
                Ok(fn_builder.ins().iconst(types::I8, 1))
            } else {
                Ok(fn_builder.ins().iconst(types::I8, 0))
            }
        }
        Literal::Float(f) => match type_ {
            Type::Primitive(PrimitiveTypes::F64) => Ok(fn_builder.ins().f64const(f)),
            Type::Primitive(PrimitiveTypes::F32) => Ok(fn_builder.ins().f32const(f as f32)),
            _ => Err(TranslateError::IncorrectTypeAnalysis { type_, node: node.clone() }),
        },
        Literal::Int(i) => match type_ {
            Type::Primitive(PrimitiveTypes::I64) | Type::Primitive(PrimitiveTypes::U64) => Ok(fn_builder.ins().iconst(types::I64, i)),
            Type::Primitive(PrimitiveTypes::I32) | Type::Primitive(PrimitiveTypes::U32) => Ok(fn_builder.ins().iconst(types::I32, i)),
            Type::Primitive(PrimitiveTypes::I16) | Type::Primitive(PrimitiveTypes::U16) => Ok(fn_builder.ins().iconst(types::I16, i)),
            Type::Primitive(PrimitiveTypes::I8) | Type::Primitive(PrimitiveTypes::U8) => Ok(fn_builder.ins().iconst(types::I8, i)),
            Type::Primitive(PrimitiveTypes::Isize) | Type::Primitive(PrimitiveTypes::Usize) => Ok(fn_builder.ins().iconst(types::I64, i)), // Assuming 64-bit target
            _ => Err(TranslateError::IncorrectTypeAnalysis { type_, node: node.clone() }),
        },
        Literal::Nil => {
            if type_ != Type::Primitive(PrimitiveTypes::Nil) {
                return Err(TranslateError::IncorrectTypeAnalysis { type_, node: node.clone() });
            }
            todo!("Nil literal not implemented");
        }
    }
}

fn expr_block(
    exprs: &[Index],
    fn_builder: &mut FunctionBuilder,
    scopes: &mut Vec<HashMap<SymbolUsize, (Type, Variable)>>,
    info: &mut Info,
) -> Result<(Value, Type), TranslateError> {
    scopes.push(HashMap::new());
    let mut last_res: Option<(Value, Type)> = None;
    for expr in exprs {
        last_res = Some(expr_to_val(*expr, fn_builder, scopes, info)?);
    }
    scopes.pop();

    Ok(last_res.unwrap_or_else(|| (fn_builder.ins().iconst(types::I32, 0), Type::Primitive(PrimitiveTypes::Nil))))
}

fn expr_call(
    func: Index,
    args: &[Index],
    fn_builder: &mut FunctionBuilder,
    scopes: &mut Vec<HashMap<SymbolUsize, (Type, Variable)>>,
    info: &mut Info,
) -> Result<Value, TranslateError> {
    let func_node = info.nodes.get(func).expect("[Function call] Function node not found in arena");
    
    let func_name = match &func_node.kind {
        NodeKind::Expr { kind: ExprKind::Identifier(sym), .. } => *sym,
        _ => panic!("Function call target must be an identifier"),
    };
    
    let func_id = *info.func_table.get(&func_name)
        .expect("Function not found in function table");
    
    let mut arg_values = Vec::new();
    for arg in args {
        let (arg_value, _arg_type) = expr_to_val(*arg, fn_builder, scopes, info)?;
        arg_values.push(arg_value);
    }
    
    let func_ref = info.module.declare_func_in_func(func_id, &mut fn_builder.func);
    
    let call_inst = fn_builder.ins().call(func_ref, &arg_values);
    
    let result = fn_builder.inst_results(call_inst)[0];
    
    Ok(result)
}
