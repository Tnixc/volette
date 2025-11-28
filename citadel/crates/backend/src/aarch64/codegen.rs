use std::collections::HashMap;

use citadel_frontend::ir::{
    self, irgen::HIRStream, ArithOpExpr, AssignStmt, BlockStmt, BrIfStmt, CallExpr, CmpOp, CmpOpExpr, ExitStmt, FuncStmt, IRExpr, IRStmt,
    JumpStmt, LabelStmt, Operator, ReturnStmt, UnaryOp, UnaryOpExpr, VarStmt,
};

use super::elements::{AArch64Element, Directive, Instruction, Label, Opcode, Operand, Register, ARG_REGS_64};

pub fn compile_program(input: HIRStream) -> Vec<AArch64Element> {
    let mut codegen = CodeGenerator::default();

    codegen.out.push(AArch64Element::Directive(Directive::Text));

    for stmt in input.stream.iter() {
        codegen.gen_stmt(stmt);
    }

    codegen.out
}

pub fn format(asm: &[AArch64Element]) -> String {
    let mut out = String::new();
    for elem in asm {
        match elem {
            AArch64Element::Directive(_) | AArch64Element::Label(_) => (),
            _ => out.push_str("    "),
        }
        out.push_str(&elem.to_string());
        out.push('\n');
    }
    out
}

#[derive(Default)]
pub struct CodeGenerator<'c> {
    pub out: Vec<AArch64Element>,
    pub symbol_table: HashMap<&'c str, i32>,
    pub stack_offset: i32,
}

impl<'c> CodeGenerator<'c> {
    fn gen_stmt(&mut self, stmt: &'c IRStmt) {
        match stmt {
            IRStmt::Entry(block) => self.gen_entry(block),
            IRStmt::Function(func) => self.gen_function(func),
            IRStmt::Variable(var) => self.gen_variable(var),
            IRStmt::Label(label) => self.gen_label(label),
            IRStmt::Return(ret) => self.gen_return(ret),
            IRStmt::Exit(exit) => self.gen_exit(exit),
            IRStmt::Jump(jmp) => self.gen_jump(jmp),
            IRStmt::Call(call) => {
                self.gen_call(call);
            }
            IRStmt::BrIf(brif) => self.gen_brif(brif),
            IRStmt::Assign(assign) => self.gen_assign(assign),
            IRStmt::DeclaredFunction(_) | IRStmt::Struct(_) | IRStmt::Union(_) => {}
        }
    }

    fn gen_entry(&mut self, block: &'c BlockStmt) {
        self.out.push(AArch64Element::Directive(Directive::Text));
        self.out.push(AArch64Element::Directive(Directive::Global("_start".into())));
        self.out.push(AArch64Element::Label(Label { name: "_start".into() }));

        for stmt in &block.stmts {
            self.gen_stmt(stmt);
        }
    }

    fn gen_function(&mut self, func: &'c FuncStmt) {
        self.stack_offset = 0;
        self.symbol_table.clear();

        let func_name = format!("_{}", func.name.ident);
        self.out.push(AArch64Element::Directive(Directive::Global(func_name.clone())));
        self.out.push(AArch64Element::Label(Label { name: func_name }));

        self.out.push(AArch64Element::Instruction(Instruction {
            opcode: Opcode::Stp,
            operands: vec![
                Operand::Register(Register::X29),
                Operand::Register(Register::X30),
                Operand::MemPreIndex(Register::Sp, -256),
            ],
        }));
        self.out.push(AArch64Element::Instruction(Instruction {
            opcode: Opcode::Mov,
            operands: vec![Operand::Register(Register::X29), Operand::Register(Register::Sp)],
        }));

        for (i, arg) in func.args.iter().enumerate() {
            if i < 8 {
                self.stack_offset -= 8;
                self.out.push(AArch64Element::Instruction(Instruction {
                    opcode: Opcode::Str,
                    operands: vec![
                        Operand::Register(ARG_REGS_64[i]),
                        Operand::MemOffset(Register::X29, self.stack_offset),
                    ],
                }));
                self.symbol_table.insert(arg.ident, self.stack_offset);
            }
        }

        for stmt in &func.block.stmts {
            self.gen_stmt(stmt);
        }

        if !matches!(
            self.out.last(),
            Some(AArch64Element::Instruction(Instruction { opcode: Opcode::Ret, .. }))
        ) {
            self.gen_epilogue();
        }
    }

    fn gen_epilogue(&mut self) {
        self.out.push(AArch64Element::Instruction(Instruction {
            opcode: Opcode::Ldp,
            operands: vec![
                Operand::Register(Register::X29),
                Operand::Register(Register::X30),
                Operand::MemPostIndex(Register::Sp, 256),
            ],
        }));
        self.out.push(AArch64Element::Instruction(Instruction {
            opcode: Opcode::Ret,
            operands: vec![],
        }));
    }

    fn gen_variable(&mut self, var: &'c VarStmt) {
        let val = self.gen_expr(&var.val);
        self.stack_offset -= 8;

        self.mov_to_reg(Register::X9, val);
        self.out.push(AArch64Element::Instruction(Instruction {
            opcode: Opcode::Str,
            operands: vec![
                Operand::Register(Register::X9),
                Operand::MemOffset(Register::X29, self.stack_offset),
            ],
        }));

        self.symbol_table.insert(var.name.ident, self.stack_offset);
    }

    fn gen_assign(&mut self, assign: &'c AssignStmt) {
        let val = self.gen_expr(&assign.val);
        let offset = *self.symbol_table.get(assign.target).unwrap();

        self.mov_to_reg(Register::X9, val);
        self.out.push(AArch64Element::Instruction(Instruction {
            opcode: Opcode::Str,
            operands: vec![Operand::Register(Register::X9), Operand::MemOffset(Register::X29, offset)],
        }));
    }

    fn gen_return(&mut self, ret: &'c ReturnStmt) {
        if let Some(ref ret_val) = ret.ret_val {
            let val = self.gen_expr(ret_val);
            self.mov_to_reg(Register::X0, val);
        }
        self.gen_epilogue();
    }

    fn gen_exit(&mut self, exit: &'c ExitStmt) {
        let code = self.gen_expr(&exit.exit_code);
        self.mov_to_reg(Register::X0, code);
        self.out.push(AArch64Element::Instruction(Instruction {
            opcode: Opcode::Mov,
            operands: vec![Operand::Register(Register::X16), Operand::Immediate(1)],
        }));
        self.out.push(AArch64Element::Instruction(Instruction {
            opcode: Opcode::Svc,
            operands: vec![Operand::Immediate(0x80)],
        }));
    }

    fn gen_label(&mut self, label: &'c LabelStmt) {
        self.out.push(AArch64Element::Label(Label {
            name: label.name.to_string(),
        }));
    }

    fn gen_jump(&mut self, jmp: &'c JumpStmt) {
        self.out.push(AArch64Element::Instruction(Instruction {
            opcode: Opcode::B,
            operands: vec![Operand::Label(jmp.label.to_string())],
        }));
    }

    fn gen_brif(&mut self, brif: &'c BrIfStmt) {
        let cond = self.gen_expr(&brif.cond);
        self.mov_to_reg(Register::X9, cond);
        self.out.push(AArch64Element::Instruction(Instruction {
            opcode: Opcode::Cmp,
            operands: vec![Operand::Register(Register::X9), Operand::Immediate(0)],
        }));
        self.out.push(AArch64Element::Instruction(Instruction {
            opcode: Opcode::BNe,
            operands: vec![Operand::Label(brif.then_label.to_string())],
        }));
        self.out.push(AArch64Element::Instruction(Instruction {
            opcode: Opcode::B,
            operands: vec![Operand::Label(brif.else_label.to_string())],
        }));
    }

    fn gen_call(&mut self, call: &'c CallExpr) -> Operand {
        for (i, arg) in call.args.iter().enumerate() {
            if i < 8 {
                let val = self.gen_expr(arg);
                self.mov_to_reg(ARG_REGS_64[i], val);
            }
        }
        self.out.push(AArch64Element::Instruction(Instruction {
            opcode: Opcode::Bl,
            operands: vec![Operand::Label(format!("_{}", call.name))],
        }));
        Operand::Register(Register::X0)
    }

    fn gen_expr(&mut self, expr: &'c IRExpr) -> Operand {
        match expr {
            IRExpr::Literal(lit, _ty) => self.gen_literal(lit),
            IRExpr::Ident(name) => {
                let offset = *self.symbol_table.get(name).unwrap();
                self.out.push(AArch64Element::Instruction(Instruction {
                    opcode: Opcode::Ldr,
                    operands: vec![Operand::Register(Register::X9), Operand::MemOffset(Register::X29, offset)],
                }));
                Operand::Register(Register::X9)
            }
            IRExpr::ArithOp(op) => self.gen_arith(op),
            IRExpr::CmpOp(cmp) => self.gen_cmp(cmp),
            IRExpr::UnaryOp(unary) => self.gen_unary(unary),
            IRExpr::Call(call) => self.gen_call(call),
            IRExpr::Cast(cast) => self.gen_expr(&cast.expr),
            IRExpr::StructInit(_) => todo!(),
        }
    }

    fn gen_literal(&mut self, lit: &ir::Literal) -> Operand {
        let val = match lit {
            ir::Literal::Int8(v) => *v as i64,
            ir::Literal::Int16(v) => *v as i64,
            ir::Literal::Int32(v) => *v as i64,
            ir::Literal::Int64(v) => *v,
            ir::Literal::Bool(v) => {
                if *v {
                    1
                } else {
                    0
                }
            }
            _ => 0,
        };
        Operand::Immediate(val)
    }

    fn gen_arith(&mut self, op: &'c ArithOpExpr) -> Operand {
        let lhs = self.gen_expr(&op.values.0);
        self.mov_to_reg(Register::X9, lhs);

        self.stack_offset -= 8;
        let save_offset = self.stack_offset;
        self.out.push(AArch64Element::Instruction(Instruction {
            opcode: Opcode::Str,
            operands: vec![Operand::Register(Register::X9), Operand::MemOffset(Register::X29, save_offset)],
        }));

        let rhs = self.gen_expr(&op.values.1);
        self.mov_to_reg(Register::X10, rhs);

        self.out.push(AArch64Element::Instruction(Instruction {
            opcode: Opcode::Ldr,
            operands: vec![Operand::Register(Register::X9), Operand::MemOffset(Register::X29, save_offset)],
        }));

        let opcode = match op.op {
            Operator::Add => Opcode::Add,
            Operator::Sub => Opcode::Sub,
            Operator::Mul => Opcode::Mul,
            Operator::Div => Opcode::Sdiv,
            Operator::Mod => {
                self.out.push(AArch64Element::Instruction(Instruction {
                    opcode: Opcode::Sdiv,
                    operands: vec![
                        Operand::Register(Register::X11),
                        Operand::Register(Register::X9),
                        Operand::Register(Register::X10),
                    ],
                }));
                self.out.push(AArch64Element::Instruction(Instruction {
                    opcode: Opcode::Mul,
                    operands: vec![
                        Operand::Register(Register::X11),
                        Operand::Register(Register::X11),
                        Operand::Register(Register::X10),
                    ],
                }));
                self.out.push(AArch64Element::Instruction(Instruction {
                    opcode: Opcode::Sub,
                    operands: vec![
                        Operand::Register(Register::X9),
                        Operand::Register(Register::X9),
                        Operand::Register(Register::X11),
                    ],
                }));
                return Operand::Register(Register::X9);
            }
            Operator::And => Opcode::And,
            Operator::Or => Opcode::Orr,
            Operator::Xor => Opcode::Eor,
            Operator::Shl => Opcode::Lsl,
            Operator::Shr => Opcode::Asr,
        };

        self.out.push(AArch64Element::Instruction(Instruction {
            opcode,
            operands: vec![
                Operand::Register(Register::X9),
                Operand::Register(Register::X9),
                Operand::Register(Register::X10),
            ],
        }));
        Operand::Register(Register::X9)
    }

    fn gen_cmp(&mut self, cmp: &'c CmpOpExpr) -> Operand {
        let lhs = self.gen_expr(&cmp.lhs);
        self.mov_to_reg(Register::X9, lhs);

        self.stack_offset -= 8;
        let save_offset = self.stack_offset;
        self.out.push(AArch64Element::Instruction(Instruction {
            opcode: Opcode::Str,
            operands: vec![Operand::Register(Register::X9), Operand::MemOffset(Register::X29, save_offset)],
        }));

        let rhs = self.gen_expr(&cmp.rhs);
        self.mov_to_reg(Register::X10, rhs);

        self.out.push(AArch64Element::Instruction(Instruction {
            opcode: Opcode::Ldr,
            operands: vec![Operand::Register(Register::X9), Operand::MemOffset(Register::X29, save_offset)],
        }));

        self.out.push(AArch64Element::Instruction(Instruction {
            opcode: Opcode::Cmp,
            operands: vec![Operand::Register(Register::X9), Operand::Register(Register::X10)],
        }));

        let cond = match cmp.op {
            CmpOp::Eq => "eq",
            CmpOp::Ne => "ne",
            CmpOp::Lt => "lt",
            CmpOp::Le => "le",
            CmpOp::Gt => "gt",
            CmpOp::Ge => "ge",
        };

        self.out.push(AArch64Element::Instruction(Instruction {
            opcode: Opcode::Cset,
            operands: vec![Operand::Register(Register::X9), Operand::Label(cond.to_string())],
        }));
        Operand::Register(Register::X9)
    }

    fn gen_unary(&mut self, unary: &'c UnaryOpExpr) -> Operand {
        match unary.op {
            UnaryOp::Neg => {
                let operand = self.gen_expr(&unary.operand);
                self.mov_to_reg(Register::X9, operand);
                self.out.push(AArch64Element::Instruction(Instruction {
                    opcode: Opcode::Neg,
                    operands: vec![Operand::Register(Register::X9), Operand::Register(Register::X9)],
                }));
                Operand::Register(Register::X9)
            }
            UnaryOp::Not => {
                let operand = self.gen_expr(&unary.operand);
                self.mov_to_reg(Register::X9, operand);
                self.out.push(AArch64Element::Instruction(Instruction {
                    opcode: Opcode::Mvn,
                    operands: vec![Operand::Register(Register::X9), Operand::Register(Register::X9)],
                }));
                Operand::Register(Register::X9)
            }
            UnaryOp::AddrOf => {
                if let IRExpr::Ident(name) = &*unary.operand {
                    let offset = *self.symbol_table.get(name).unwrap();
                    self.out.push(AArch64Element::Instruction(Instruction {
                        opcode: Opcode::Add,
                        operands: vec![
                            Operand::Register(Register::X9),
                            Operand::Register(Register::X29),
                            Operand::Immediate(offset as i64),
                        ],
                    }));
                    Operand::Register(Register::X9)
                } else {
                    let operand = self.gen_expr(&unary.operand);
                    self.mov_to_reg(Register::X9, operand);
                    Operand::Register(Register::X9)
                }
            }
            UnaryOp::Deref => {
                let operand = self.gen_expr(&unary.operand);
                self.mov_to_reg(Register::X9, operand);
                self.out.push(AArch64Element::Instruction(Instruction {
                    opcode: Opcode::Ldr,
                    operands: vec![Operand::Register(Register::X9), Operand::MemBase(Register::X9)],
                }));
                Operand::Register(Register::X9)
            }
        }
    }

    fn mov_to_reg(&mut self, reg: Register, val: Operand) {
        match val {
            Operand::Register(r) if r == reg => {}
            Operand::Immediate(v) => {
                self.out.push(AArch64Element::Instruction(Instruction {
                    opcode: Opcode::Mov,
                    operands: vec![Operand::Register(reg), Operand::Immediate(v)],
                }));
            }
            _ => {
                self.out.push(AArch64Element::Instruction(Instruction {
                    opcode: Opcode::Mov,
                    operands: vec![Operand::Register(reg), val],
                }));
            }
        }
    }
}
