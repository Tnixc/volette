use std::collections::HashMap;

use generational_arena::Arena;
use string_interner::{StringInterner, backend::BucketBackend, symbol::SymbolUsize};

use crate::{
    SafeConvert,
    compiler::{
        analysis::context::TypeCtx,
        error::{Help, ReportCollection},
        parser::node::{DefKind, Node, NodeKind, VType},
        tokens::PrimitiveTypes,
    },
};

pub fn type_check_root(
    root: &Node,
    interner: &StringInterner<BucketBackend<SymbolUsize>>,
    nodes: &mut Arena<Node>,
    fn_table: &HashMap<SymbolUsize, (Vec<VType>, VType)>,
) -> Result<(), ReportCollection> {
    let mut diagnostics = ReportCollection::new();

    if let NodeKind::Root { defs } = &root.kind {
        for idx in defs {
            let (body_idx, params, return_type) = {
                let node = nodes.get(*idx).safe();
                if let NodeKind::Def { kind } = &node.kind {
                    if let DefKind::Function {
                        body, params, return_type, ..
                    } = kind
                    {
                        (*body, params.clone(), return_type.clone())
                    } else {
                        diagnostics.push(
                            crate::analysis_err!("Ts Unsupported vro: {}", Some(node.span.to_display(interner)), node,).into_cloneable(),
                        );
                        continue;
                    }
                } else {
                    continue;
                }
            };

            let mut ctx = TypeCtx::new(nodes, interner, fn_table, &mut diagnostics);

            for param in &params {
                if ctx.ident_types.contains_key(&param.0) {
                    ctx.diagnostics.push(
                        crate::analysis_err!(
                            "Duplicate parameter name '{}'",
                            Some(param.2.to_display(ctx.interner)),
                            ctx.interner.resolve(param.0).safe()
                        )
                        .attach(Help("Parameter names must be unique within a function".into()))
                        .into_cloneable(),
                    );
                }

                ctx.ident_types.insert(param.0, param.1.clone());
            }

            let error_count_before = ctx.diagnostics.len();
            if let Err(e) = ctx.resolve_expr_type(body_idx, None) {
                ctx.diagnostics.push(e.into_cloneable());
            }
            let had_body_errors = ctx.diagnostics.len() > error_count_before;

            if !had_body_errors {
                let body_node = ctx.nodes.get(body_idx).safe();
                if let NodeKind::Expr {
                    type_: Some(body_type), ..
                } = &body_node.kind
                {
                    if *body_type != VType::Primitive(PrimitiveTypes::Never) && *body_type != return_type {
                        ctx.diagnostics.push(
                            crate::analysis_err!(
                                "Type mismatch: expected {:?}, got {:?}",
                                Some(body_node.span.to_display(ctx.interner)),
                                return_type,
                                body_type
                            )
                            .attach(Help("Ensure the types match or add an explicit conversion".into()))
                            .into_cloneable(),
                        );
                    }
                }
            }
        }
    }

    if !diagnostics.is_empty() { Err(diagnostics) } else { Ok(()) }
}
