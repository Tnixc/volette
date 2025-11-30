//! Lowering from Volette AST to Citadel HIR

use std::collections::HashMap;

use bumpalo::Bump;
use citadel_api::frontend::ir::{
    self, ArithOpExpr, AssignStmt, BlockStmt, BrIfStmt, CallExpr, CastExpr, CmpOp, CmpOpExpr, FuncStmt, INT8_T, INT32_T, INT64_T, IRExpr,
    IRStmt, IRTypedIdent, JumpStmt, LabelStmt, Literal, Operator, ReturnStmt, Type, UnaryOp, UnaryOpExpr, VarStmt,
    irgen::{HIRStream, IRGenerator},
};
use generational_arena::{Arena, Index};
use rootcause::prelude::*;
use string_interner::{StringInterner, backend::BucketBackend, symbol::SymbolUsize};

use crate::compiler::parser::node::{BinOpKind, DefKind, ExprKind, Node, NodeKind, UnaryOpKind, VType};

struct LoopContext<'a> {
    break_label: &'a str,
    continue_label: &'a str,
}

pub struct CitadelLowering<'a> {
    arena: &'a Bump,
    nodes: &'a Arena<Node>,
    interner: &'a StringInterner<BucketBackend<SymbolUsize>>,
    ir_gen: IRGenerator<'a>,
    label_counter: usize,
    scopes: Vec<HashMap<SymbolUsize, (&'a str, Type<'a>)>>,
    var_counter: usize,
    loop_stack: Vec<LoopContext<'a>>,
}

impl<'a> CitadelLowering<'a> {
    pub fn new(arena: &'a Bump, nodes: &'a Arena<Node>, interner: &'a StringInterner<BucketBackend<SymbolUsize>>) -> Self {
        Self {
            arena,
            nodes,
            interner,
            ir_gen: IRGenerator::default(),
            label_counter: 0,
            scopes: Vec::new(),
            var_counter: 0,
            loop_stack: Vec::new(),
        }
    }

    fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn is_diverging(&self, idx: Index) -> bool {
        let node = self.nodes.get(idx).unwrap();
        match &node.kind {
            NodeKind::Expr { kind, .. } => matches!(kind, ExprKind::Return { .. } | ExprKind::Break | ExprKind::Continue),
            _ => false,
        }
    }

    fn declare_var(&mut self, sym: SymbolUsize, ty: Type<'a>) -> &'a str {
        let base_name = self.interner.resolve(sym).unwrap_or("var");
        let mangled = self.arena.alloc_str(&format!("{}_{}", base_name, self.var_counter));
        self.var_counter += 1;
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(sym, (mangled, ty));
        }
        mangled
    }

    fn lookup_var(&self, sym: SymbolUsize) -> Option<(&'a str, Type<'a>)> {
        for scope in self.scopes.iter().rev() {
            if let Some(entry) = scope.get(&sym) {
                return Some(*entry);
            }
        }
        None
    }

    fn fresh_label(&mut self, prefix: &str) -> &'a str {
        let label = self.arena.alloc_str(&format!("{}_{}", prefix, self.label_counter));
        self.label_counter += 1;
        label
    }

    fn resolve_symbol(&self, sym: SymbolUsize) -> &'a str {
        let s = self.interner.resolve(sym).unwrap_or("<unknown>");
        self.arena.alloc_str(s)
    }

    pub fn lower(mut self, root: &Node) -> Result<HIRStream<'a>, Report> {
        match &root.kind {
            NodeKind::Root { defs } => {
                for def_idx in defs {
                    let def_node = self
                        .nodes
                        .get(*def_idx)
                        .ok_or_else(|| crate::codegen_err!("Invalid node index", None))?;
                    self.lower_def(def_node)?;
                }
            }
            _ => {
                return Err(crate::codegen_err!("Expected root node", Some(root.span.to_display(self.interner))));
            }
        }

        Ok(self.ir_gen.stream())
    }

    fn lower_def(&mut self, node: &Node) -> Result<(), Report> {
        match &node.kind {
            NodeKind::Def { kind } => match kind {
                DefKind::Function {
                    name,
                    params,
                    body,
                    return_type,
                } => {
                    self.lower_function(*name, params, *body, return_type)?;
                }
                DefKind::Struct { .. } => {
                    // Skip struct definitions for now
                }
            },
            _ => {
                return Err(crate::codegen_err!(
                    "Expected definition node",
                    Some(node.span.to_display(self.interner))
                ));
            }
        }
        Ok(())
    }

    fn lower_function(
        &mut self,
        name: SymbolUsize,
        params: &[(SymbolUsize, VType, crate::compiler::tokens::Span)],
        body: Index,
        return_type: &VType,
    ) -> Result<(), Report> {
        let fn_name = self.resolve_symbol(name);
        let ret_type = self.lower_type(return_type);

        self.push_scope();

        let args: Vec<IRTypedIdent<'a>> = params
            .iter()
            .map(|(sym, ty, _)| {
                let ty_ir = self.lower_type(ty);
                let mangled = self.declare_var(*sym, ty_ir.clone());
                IRTypedIdent {
                    ident: mangled,
                    _type: ty_ir,
                }
            })
            .collect();

        let block = self.lower_function_body(body, return_type)?;

        self.pop_scope();

        self.ir_gen.gen_ir(IRStmt::Function(FuncStmt {
            name: IRTypedIdent {
                ident: fn_name,
                _type: ret_type,
            },
            args,
            block,
        }));

        Ok(())
    }

    fn lower_function_body(&mut self, body_idx: Index, return_type: &VType) -> Result<BlockStmt<'a>, Report> {
        let body_node = self
            .nodes
            .get(body_idx)
            .ok_or_else(|| crate::codegen_err!("Invalid body index", None))?;

        match &body_node.kind {
            NodeKind::Expr {
                kind: ExprKind::Block { exprs },
                ..
            } => {
                let mut stmts = Vec::new();
                let len = exprs.len();

                for (i, expr_idx) in exprs.iter().enumerate() {
                    let is_last = i == len - 1;

                    if is_last && !return_type.is_zst() {
                        if self.is_diverging(*expr_idx) {
                            let expr_stmts = self.lower_expr_to_stmts_inner(*expr_idx)?;
                            stmts.extend(expr_stmts);
                        } else {
                            let (prereqs, expr) = self.lower_expr_with_prereqs(*expr_idx)?;
                            stmts.extend(prereqs);
                            stmts.push(IRStmt::Return(ReturnStmt { ret_val: Some(expr) }));
                        }
                    } else {
                        let expr_stmts = self.lower_expr_to_stmts_inner(*expr_idx)?;
                        stmts.extend(expr_stmts);
                    }
                }
                Ok(BlockStmt { stmts })
            }
            _ => {
                if !return_type.is_zst() {
                    // Check if body already diverges
                    if self.is_diverging(body_idx) {
                        let stmts = self.lower_expr_to_stmts_inner(body_idx)?;
                        Ok(BlockStmt { stmts })
                    } else {
                        let (prereqs, expr) = self.lower_expr_with_prereqs(body_idx)?;
                        let mut stmts = prereqs;
                        stmts.push(IRStmt::Return(ReturnStmt { ret_val: Some(expr) }));
                        Ok(BlockStmt { stmts })
                    }
                } else {
                    let stmts = self.lower_expr_to_stmts_inner(body_idx)?;
                    Ok(BlockStmt { stmts })
                }
            }
        }
    }

    fn lower_block(&mut self, body_idx: Index) -> Result<BlockStmt<'a>, Report> {
        let body_node = self
            .nodes
            .get(body_idx)
            .ok_or_else(|| crate::codegen_err!("Invalid body index", None))?;

        match &body_node.kind {
            NodeKind::Expr {
                kind: ExprKind::Block { exprs },
                ..
            } => {
                let mut stmts = Vec::new();
                for expr_idx in exprs {
                    let expr_stmts = self.lower_expr_to_stmts_inner(*expr_idx)?;
                    stmts.extend(expr_stmts);
                }
                Ok(BlockStmt { stmts })
            }
            _ => {
                let stmts = self.lower_expr_to_stmts_inner(body_idx)?;
                Ok(BlockStmt { stmts })
            }
        }
    }

    fn lower_expr_with_prereqs(&mut self, idx: Index) -> Result<(Vec<IRStmt<'a>>, IRExpr<'a>), Report> {
        let node = self
            .nodes
            .get(idx)
            .ok_or_else(|| crate::codegen_err!("Invalid expression index", None))?;

        match &node.kind {
            NodeKind::Expr { kind, type_ } => match kind {
                ExprKind::Literal(lit) => Ok((vec![], self.lower_literal_inner(*lit, type_.as_ref())?)),

                ExprKind::Identifier(sym) => {
                    let name = self.lookup_var(*sym).map(|(n, _)| n).unwrap_or_else(|| self.resolve_symbol(*sym));
                    Ok((vec![], IRExpr::Ident(name)))
                }

                ExprKind::BinOp { left, right, op } => {
                    let (stmts, expr) = self.lower_binop_with_prereqs(*left, *right, *op)?;
                    Ok((stmts, expr))
                }

                ExprKind::UnaryOp { op, expr } => {
                    let (stmts, inner) = self.lower_expr_with_prereqs(*expr)?;
                    let unary = self.lower_unaryop_inner(*op, inner)?;
                    Ok((stmts, unary))
                }

                ExprKind::Call { func, args } => {
                    let mut all_stmts = Vec::new();
                    let call = self.lower_call_with_prereqs(*func, args, &mut all_stmts)?;
                    Ok((all_stmts, IRExpr::Call(call)))
                }

                ExprKind::Cast { expr, target_type } => {
                    let (stmts, inner) = self.lower_expr_with_prereqs(*expr)?;
                    Ok((
                        stmts,
                        IRExpr::Cast(CastExpr {
                            expr: Box::new(inner),
                            to_type: self.lower_type(target_type),
                        }),
                    ))
                }

                ExprKind::ParenExpr { expr } => self.lower_expr_with_prereqs(*expr),

                ExprKind::Block { exprs } => {
                    if exprs.is_empty() {
                        return Ok((vec![], IRExpr::Literal(Literal::Int32(0), Type::Ident(INT32_T))));
                    }

                    self.push_scope();
                    let mut all_stmts = Vec::new();

                    for expr_idx in &exprs[..exprs.len() - 1] {
                        let stmts = self.lower_expr_to_stmts_inner(*expr_idx)?;
                        all_stmts.extend(stmts);
                    }

                    let last_idx = exprs.last().unwrap();
                    let (last_prereqs, last_expr) = self.lower_expr_with_prereqs(*last_idx)?;
                    all_stmts.extend(last_prereqs);

                    self.pop_scope();
                    Ok((all_stmts, last_expr))
                }

                ExprKind::BlockReturn { value } => {
                    if let Some(val_idx) = value {
                        self.lower_expr_with_prereqs(*val_idx)
                    } else {
                        Ok((vec![], IRExpr::Literal(Literal::Int32(0), Type::Ident(INT32_T))))
                    }
                }

                ExprKind::Assign { target, value } => {
                    let target_node = self
                        .nodes
                        .get(*target)
                        .ok_or_else(|| crate::codegen_err!("Invalid assign target", None))?;

                    let target_name = match &target_node.kind {
                        NodeKind::Expr {
                            kind: ExprKind::Identifier(sym),
                            ..
                        } => self.lookup_var(*sym).map(|(n, _)| n).unwrap_or_else(|| self.resolve_symbol(*sym)),
                        _ => {
                            return Err(crate::codegen_err!(
                                "Assignment target must be identifier",
                                Some(target_node.span.to_display(self.interner))
                            ));
                        }
                    };

                    let (mut stmts, val) = self.lower_expr_with_prereqs(*value)?;

                    stmts.push(IRStmt::Assign(AssignStmt {
                        target: target_name,
                        val: val.clone(),
                    }));

                    Ok((stmts, IRExpr::Ident(target_name)))
                }

                ExprKind::LetBinding { name, value, .. } => {
                    let (mut stmts, val) = self.lower_expr_with_prereqs(*value)?;
                    let var_type = type_.as_ref().map(|t| self.lower_type(t)).unwrap_or(Type::Ident(INT32_T));

                    if type_.as_ref().map(|t| t.is_zst()).unwrap_or(false) {
                        if let IRExpr::Call(call) = val {
                            stmts.push(IRStmt::Call(call));
                        }
                        return Ok((stmts, IRExpr::Literal(Literal::Int32(0), Type::Ident(INT32_T))));
                    }

                    let var_name = self.declare_var(*name, var_type.clone());

                    stmts.push(IRStmt::Variable(VarStmt {
                        name: IRTypedIdent {
                            ident: var_name,
                            _type: var_type,
                        },
                        val: val.clone(),
                        is_const: false,
                    }));

                    Ok((stmts, IRExpr::Ident(var_name)))
                }

                ExprKind::Return { value } => {
                    // return is a diverging expression - emit return statement and return a dummy value
                    // i've replaced dummy values with 555, 777, etc to see if they show up
                    // hopefully not
                    if let Some(val_idx) = value {
                        let val_node = self.nodes.get(*val_idx);
                        let is_zst = val_node
                            .and_then(|n| {
                                if let NodeKind::Expr { type_, .. } = &n.kind {
                                    type_.as_ref().map(|t| t.is_zst())
                                } else {
                                    None
                                }
                            })
                            .unwrap_or(false);

                        if is_zst {
                            return Ok((
                                vec![IRStmt::Return(ReturnStmt { ret_val: None })],
                                IRExpr::Literal(Literal::Int32(333), Type::Ident(INT32_T)),
                            ));
                        }

                        let (mut stmts, expr) = self.lower_expr_with_prereqs(*val_idx)?;
                        stmts.push(IRStmt::Return(ReturnStmt { ret_val: Some(expr) }));
                        Ok((stmts, IRExpr::Literal(Literal::Int32(444), Type::Ident(INT32_T))))
                    } else {
                        Ok((
                            vec![IRStmt::Return(ReturnStmt { ret_val: None })],
                            IRExpr::Literal(Literal::Int32(555), Type::Ident(INT32_T)),
                        ))
                    }
                }

                ExprKind::If {
                    cond,
                    then_block,
                    else_block,
                } => {
                    let stmts = self.lower_if(*cond, *then_block, *else_block)?;
                    Ok((stmts, IRExpr::Literal(Literal::Int32(777), Type::Ident(INT32_T))))
                }

                ExprKind::While { cond, body } => {
                    let stmts = self.lower_while(*cond, *body)?;
                    Ok((stmts, IRExpr::Literal(Literal::Int32(888), Type::Ident(INT32_T))))
                }

                ExprKind::Break => {
                    let ctx = self
                        .loop_stack
                        .last()
                        .ok_or_else(|| crate::codegen_err!("'break' outside of loop", Some(node.span.to_display(self.interner))))?;
                    Ok((
                        vec![IRStmt::Jump(JumpStmt { label: ctx.break_label })],
                        IRExpr::Literal(Literal::Int32(0), Type::Ident(INT32_T)),
                    ))
                }

                ExprKind::Continue => {
                    let ctx = self
                        .loop_stack
                        .last()
                        .ok_or_else(|| crate::codegen_err!("'continue' outside of loop", Some(node.span.to_display(self.interner))))?;
                    Ok((
                        vec![IRStmt::Jump(JumpStmt { label: ctx.continue_label })],
                        IRExpr::Literal(Literal::Int32(0), Type::Ident(INT32_T)),
                    ))
                }

                _ => Err(crate::codegen_err!(
                    "Unsupported expression kind: {:?}",
                    Some(node.span.to_display(self.interner)),
                    kind
                )),
            },
            _ => Err(crate::codegen_err!(
                "Expected expression node",
                Some(node.span.to_display(self.interner))
            )),
        }
    }

    fn lower_literal_inner(
        &mut self,
        lit: crate::compiler::parser::node::Literal,
        type_hint: Option<&VType>,
    ) -> Result<IRExpr<'a>, Report> {
        use crate::compiler::parser::node::Literal as VLit;

        match lit {
            VLit::Int(v) => {
                let ty = type_hint.map(|t| self.lower_type(t)).unwrap_or(Type::Ident(INT32_T));

                match ty {
                    Type::Ident(INT8_T) => Ok(IRExpr::Literal(Literal::Int8(v as i8), ty)),
                    Type::Ident(INT64_T) => Ok(IRExpr::Literal(Literal::Int64(v), ty)),
                    _ => Ok(IRExpr::Literal(Literal::Int32(v as i32), ty)),
                }
            }
            VLit::Float(v) => Ok(IRExpr::Literal(Literal::Float64(v), Type::Ident(ir::FLOAT64_T))),
            VLit::Bool(v) => Ok(IRExpr::Literal(Literal::Bool(v), Type::Ident(INT8_T))),
        }
    }

    fn lower_unaryop_inner(&mut self, op: UnaryOpKind, operand: IRExpr<'a>) -> Result<IRExpr<'a>, Report> {
        match op {
            UnaryOpKind::Neg => Ok(IRExpr::UnaryOp(UnaryOpExpr {
                op: UnaryOp::Neg,
                operand: Box::new(operand),
            })),
            UnaryOpKind::Not => Ok(IRExpr::UnaryOp(UnaryOpExpr {
                op: UnaryOp::Not,
                operand: Box::new(operand),
            })),
            UnaryOpKind::AddressOf => Ok(IRExpr::UnaryOp(UnaryOpExpr {
                op: UnaryOp::AddrOf,
                operand: Box::new(operand),
            })),
            UnaryOpKind::Deref => Ok(IRExpr::UnaryOp(UnaryOpExpr {
                op: UnaryOp::Deref,
                operand: Box::new(operand),
            })),
        }
    }

    fn lower_binop_with_prereqs(&mut self, left: Index, right: Index, op: BinOpKind) -> Result<(Vec<IRStmt<'a>>, IRExpr<'a>), Report> {
        let (mut stmts, left_expr) = self.lower_expr_with_prereqs(left)?;
        let (right_stmts, right_expr) = self.lower_expr_with_prereqs(right)?;
        stmts.extend(right_stmts);

        let expr = self.make_binop_expr(left_expr, right_expr, op)?;
        Ok((stmts, expr))
    }

    fn make_binop_expr(&self, left: IRExpr<'a>, right: IRExpr<'a>, op: BinOpKind) -> Result<IRExpr<'a>, Report> {
        match op {
            BinOpKind::Add => Ok(IRExpr::ArithOp(ArithOpExpr {
                op: Operator::Add,
                values: (Box::new(left), Box::new(right)),
            })),
            BinOpKind::Sub => Ok(IRExpr::ArithOp(ArithOpExpr {
                op: Operator::Sub,
                values: (Box::new(left), Box::new(right)),
            })),
            BinOpKind::Mul => Ok(IRExpr::ArithOp(ArithOpExpr {
                op: Operator::Mul,
                values: (Box::new(left), Box::new(right)),
            })),
            BinOpKind::Div => Ok(IRExpr::ArithOp(ArithOpExpr {
                op: Operator::Div,
                values: (Box::new(left), Box::new(right)),
            })),
            BinOpKind::Mod => Ok(IRExpr::ArithOp(ArithOpExpr {
                op: Operator::Mod,
                values: (Box::new(left), Box::new(right)),
            })),
            BinOpKind::Eq => Ok(IRExpr::CmpOp(CmpOpExpr {
                op: CmpOp::Eq,
                lhs: Box::new(left),
                rhs: Box::new(right),
            })),
            BinOpKind::NotEq => Ok(IRExpr::CmpOp(CmpOpExpr {
                op: CmpOp::Ne,
                lhs: Box::new(left),
                rhs: Box::new(right),
            })),
            BinOpKind::LessThan => Ok(IRExpr::CmpOp(CmpOpExpr {
                op: CmpOp::Lt,
                lhs: Box::new(left),
                rhs: Box::new(right),
            })),
            BinOpKind::LessThanOrEq => Ok(IRExpr::CmpOp(CmpOpExpr {
                op: CmpOp::Le,
                lhs: Box::new(left),
                rhs: Box::new(right),
            })),
            BinOpKind::GreaterThan => Ok(IRExpr::CmpOp(CmpOpExpr {
                op: CmpOp::Gt,
                lhs: Box::new(left),
                rhs: Box::new(right),
            })),
            BinOpKind::GreaterThanOrEq => Ok(IRExpr::CmpOp(CmpOpExpr {
                op: CmpOp::Ge,
                lhs: Box::new(left),
                rhs: Box::new(right),
            })),
            BinOpKind::LogicalAnd => Ok(IRExpr::ArithOp(ArithOpExpr {
                op: Operator::And,
                values: (Box::new(left), Box::new(right)),
            })),
            BinOpKind::LogicalOr => Ok(IRExpr::ArithOp(ArithOpExpr {
                op: Operator::Or,
                values: (Box::new(left), Box::new(right)),
            })),
            BinOpKind::BitwiseAnd => Ok(IRExpr::ArithOp(ArithOpExpr {
                op: Operator::And,
                values: (Box::new(left), Box::new(right)),
            })),
            BinOpKind::BitwiseOr => Ok(IRExpr::ArithOp(ArithOpExpr {
                op: Operator::Or,
                values: (Box::new(left), Box::new(right)),
            })),
            BinOpKind::BitwiseXor => Ok(IRExpr::ArithOp(ArithOpExpr {
                op: Operator::Xor,
                values: (Box::new(left), Box::new(right)),
            })),
            _ => Err(crate::codegen_err!("Unsupported binary operator: {:?}", None, op)),
        }
    }

    fn lower_call_with_prereqs(&mut self, func: Index, args: &[Index], prereq_stmts: &mut Vec<IRStmt<'a>>) -> Result<CallExpr<'a>, Report> {
        let func_node = self
            .nodes
            .get(func)
            .ok_or_else(|| crate::codegen_err!("Invalid function index", None))?;

        let func_name = match &func_node.kind {
            NodeKind::Expr {
                kind: ExprKind::Identifier(sym),
                ..
            } => self.resolve_symbol(*sym),
            _ => {
                return Err(crate::codegen_err!(
                    "Expected function identifier",
                    Some(func_node.span.to_display(self.interner))
                ));
            }
        };

        let mut arg_exprs = Vec::new();
        for arg_idx in args {
            let (stmts, arg_expr) = self.lower_expr_with_prereqs(*arg_idx)?;
            prereq_stmts.extend(stmts);
            arg_exprs.push(arg_expr);
        }

        Ok(CallExpr {
            name: func_name,
            args: arg_exprs,
        })
    }

    fn lower_expr_to_stmts_inner(&mut self, idx: Index) -> Result<Vec<IRStmt<'a>>, Report> {
        let node = self
            .nodes
            .get(idx)
            .ok_or_else(|| crate::codegen_err!("Invalid expression index", None))?;

        match &node.kind {
            NodeKind::Expr { kind, type_ } => {
                match kind {
                    ExprKind::Return { value } => {
                        if let Some(val_idx) = value {
                            let val_node = self.nodes.get(*val_idx);
                            let is_zst = val_node
                                .and_then(|n| {
                                    if let NodeKind::Expr { type_, .. } = &n.kind {
                                        type_.as_ref().map(|t| t.is_zst())
                                    } else {
                                        None
                                    }
                                })
                                .unwrap_or(false);

                            if is_zst {
                                return Ok(vec![IRStmt::Return(ReturnStmt { ret_val: None })]);
                            }

                            let (stmts, expr) = self.lower_expr_with_prereqs(*val_idx)?;
                            let mut result = stmts;
                            result.push(IRStmt::Return(ReturnStmt { ret_val: Some(expr) }));
                            return Ok(result);
                        }
                        Ok(vec![IRStmt::Return(ReturnStmt { ret_val: None })])
                    }

                    ExprKind::LetBinding { name, value, .. } => {
                        if type_.as_ref().map(|t| t.is_zst()).unwrap_or(false) {
                            let (mut stmts, val) = self.lower_expr_with_prereqs(*value)?;
                            if let IRExpr::Call(call) = val {
                                stmts.push(IRStmt::Call(call));
                            }
                            return Ok(stmts);
                        }

                        let (mut stmts, val) = self.lower_expr_with_prereqs(*value)?;
                        let var_type = type_.as_ref().map(|t| self.lower_type(t)).unwrap_or(Type::Ident(INT32_T));
                        let var_name = self.declare_var(*name, var_type.clone());

                        stmts.push(IRStmt::Variable(VarStmt {
                            name: IRTypedIdent {
                                ident: var_name,
                                _type: var_type,
                            },
                            val,
                            is_const: false,
                        }));
                        Ok(stmts)
                    }

                    ExprKind::Assign { target, value } => {
                        let target_node = self
                            .nodes
                            .get(*target)
                            .ok_or_else(|| crate::codegen_err!("Invalid assign target", None))?;

                        let target_name = match &target_node.kind {
                            NodeKind::Expr {
                                kind: ExprKind::Identifier(sym),
                                ..
                            } => self.lookup_var(*sym).map(|(n, _)| n).unwrap_or_else(|| self.resolve_symbol(*sym)),
                            _ => {
                                return Err(crate::codegen_err!(
                                    "Assignment target must be identifier",
                                    Some(target_node.span.to_display(self.interner))
                                ));
                            }
                        };

                        let (mut stmts, val) = self.lower_expr_with_prereqs(*value)?;
                        stmts.push(IRStmt::Assign(AssignStmt { target: target_name, val }));
                        Ok(stmts)
                    }

                    ExprKind::If {
                        cond,
                        then_block,
                        else_block,
                    } => self.lower_if(*cond, *then_block, *else_block),

                    ExprKind::While { cond, body } => self.lower_while(*cond, *body),

                    ExprKind::Break => {
                        let ctx = self
                            .loop_stack
                            .last()
                            .ok_or_else(|| crate::codegen_err!("'break' outside of loop", Some(node.span.to_display(self.interner))))?;
                        Ok(vec![IRStmt::Jump(JumpStmt { label: ctx.break_label })])
                    }

                    ExprKind::Continue => {
                        let ctx = self
                            .loop_stack
                            .last()
                            .ok_or_else(|| crate::codegen_err!("'continue' outside of loop", Some(node.span.to_display(self.interner))))?;
                        Ok(vec![IRStmt::Jump(JumpStmt { label: ctx.continue_label })])
                    }

                    ExprKind::Block { exprs } => {
                        self.push_scope();
                        let mut stmts = Vec::new();
                        for expr_idx in exprs {
                            let diverges = self.is_diverging(*expr_idx);
                            let expr_stmts = self.lower_expr_to_stmts_inner(*expr_idx)?;
                            stmts.extend(expr_stmts);
                            if diverges {
                                break;
                            }
                        }
                        self.pop_scope();
                        Ok(stmts)
                    }

                    ExprKind::Call { func, args } => {
                        let mut prereqs = Vec::new();
                        let call = self.lower_call_with_prereqs(*func, args, &mut prereqs)?;
                        prereqs.push(IRStmt::Call(call));
                        Ok(prereqs)
                    }

                    _ => {
                        // Other expressions: evaluate for side effects
                        let (stmts, _) = self.lower_expr_with_prereqs(idx)?;
                        Ok(stmts)
                    }
                }
            }
            _ => Err(crate::codegen_err!(
                "Expected expression node",
                Some(node.span.to_display(self.interner))
            )),
        }
    }

    fn lower_if(&mut self, cond: Index, then_block: Index, else_block: Option<Index>) -> Result<Vec<IRStmt<'a>>, Report> {
        let then_label = self.fresh_label("then");
        let else_label = self.fresh_label("else");
        let end_label = self.fresh_label("endif");

        let mut stmts = Vec::new();

        // Condition and branch
        let (cond_prereqs, cond_expr) = self.lower_expr_with_prereqs(cond)?;
        stmts.extend(cond_prereqs);
        stmts.push(IRStmt::BrIf(BrIfStmt {
            cond: cond_expr,
            then_label,
            else_label,
        }));

        // Then block
        stmts.push(IRStmt::Label(LabelStmt { name: then_label }));
        let then_stmts = self.lower_block(then_block)?;
        stmts.extend(then_stmts.stmts);
        stmts.push(IRStmt::Jump(JumpStmt { label: end_label }));

        // Else block
        stmts.push(IRStmt::Label(LabelStmt { name: else_label }));
        if let Some(else_idx) = else_block {
            let else_stmts = self.lower_block(else_idx)?;
            stmts.extend(else_stmts.stmts);
        }
        stmts.push(IRStmt::Jump(JumpStmt { label: end_label }));

        // End label
        stmts.push(IRStmt::Label(LabelStmt { name: end_label }));

        Ok(stmts)
    }

    fn lower_while(&mut self, cond: Index, body: Index) -> Result<Vec<IRStmt<'a>>, Report> {
        let loop_start = self.fresh_label("while_start");
        let loop_body = self.fresh_label("while_body");
        let loop_end = self.fresh_label("while_end");

        self.loop_stack.push(LoopContext {
            break_label: loop_end,
            continue_label: loop_start,
        });

        let mut stmts = Vec::new();

        // Loop start label
        stmts.push(IRStmt::Label(LabelStmt { name: loop_start }));

        // Condition check
        let (cond_prereqs, cond_expr) = self.lower_expr_with_prereqs(cond)?;
        stmts.extend(cond_prereqs);
        stmts.push(IRStmt::BrIf(BrIfStmt {
            cond: cond_expr,
            then_label: loop_body,
            else_label: loop_end,
        }));

        // Loop body
        stmts.push(IRStmt::Label(LabelStmt { name: loop_body }));
        let body_stmts = self.lower_block(body)?;
        stmts.extend(body_stmts.stmts);
        stmts.push(IRStmt::Jump(JumpStmt { label: loop_start }));

        // Loop end
        stmts.push(IRStmt::Label(LabelStmt { name: loop_end }));

        self.loop_stack.pop();

        Ok(stmts)
    }

    fn lower_type(&mut self, ty: &VType) -> Type<'a> {
        match ty {
            VType::Primitive(p) => Type::Ident(p.to_citadel_str()),
            VType::Pointer(_) => Type::Ident(INT64_T),
            _ => {
                todo!()
            }
        }
    }
}
