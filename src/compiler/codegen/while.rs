use cranelift::prelude::{FunctionBuilder, InstBuilder};
use generational_arena::Index;
use rootcause::Report;

use crate::SafeConvert;
use crate::compiler::codegen::block::expr_block;
use crate::compiler::{
    codegen::{Info, Scopes, expr::expr_to_val},
    parser::node::{ExprKind, NodeKind, VType},
    tokens::PrimitiveTypes,
};

pub fn expr_while(while_node: Index, fn_builder: &mut FunctionBuilder, scopes: &mut Scopes, info: &mut Info) -> Result<(), Report> {
    let while_node = info.nodes.get(while_node).safe();
    match while_node.kind {
        NodeKind::Expr {
            kind: ExprKind::While { cond, body },
            ..
        } => {
            // basically
            // current block -> jump to loop_header
            // loop_header -> evaluate condition, branch to body or exit
            // loop_body -> execute body, jump back to header
            // loop_exit -> continue after loop

            let loop_header = fn_builder.create_block();
            let loop_body = fn_builder.create_block();
            let loop_exit = fn_builder.create_block();

            // jump from current block to loop header
            fn_builder.ins().jump(loop_header, &[]);

            // switch to loop header and evaluate condition
            fn_builder.switch_to_block(loop_header);
            let cond_val = expr_to_val(cond, fn_builder, scopes, info)?;

            if cond_val.1 != VType::Primitive(PrimitiveTypes::Bool) {
                return Err(crate::codegen_err!(
                    "Type mismatch: expected bool type for while condition but got type {:?}",
                    Some(while_node.span.to_display(info.interner)),
                    cond_val.1
                ));
            }

            let cond_val = cond_val.0.safe();
            fn_builder.ins().brif(cond_val, loop_body, &[], loop_exit, &[]);

            // switch to loop body and execute it
            fn_builder.switch_to_block(loop_body);
            let loop_content = match &info.nodes.get(body).safe().kind {
                NodeKind::Expr {
                    kind: ExprKind::Block { exprs },
                    ..
                } => exprs,
                _ => unreachable!(),
            };
            expr_block(&loop_content, fn_builder, scopes, info)?;

            // jump back to loop header for next iteration
            fn_builder.ins().jump(loop_header, &[]);

            fn_builder.seal_block(loop_body);
            fn_builder.seal_block(loop_header);

            // switch to loop exit to continue after the loop
            fn_builder.switch_to_block(loop_exit);
            fn_builder.seal_block(loop_exit);

            Ok(())
        }
        _ => unreachable!(),
    }
}
