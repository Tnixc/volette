use std::fmt::Display;

#[derive(Debug, Clone, PartialEq)]
pub enum AArch64Element {
    Label(Label),
    Instruction(Instruction),
    Directive(Directive),
    Declaration(Declaration),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Label {
    pub name: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Instruction {
    pub opcode: Opcode,
    pub operands: Vec<Operand>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Directive {
    Global(String),
    Text,
    Data,
    Align(u32),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Declaration {
    Word(String, i64),
    Byte(String, i8),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Opcode {
    Mov,
    Movz,
    Movk,
    Add,
    Sub,
    Mul,
    Sdiv,
    Udiv,
    And,
    Orr,
    Eor,
    Lsl,
    Lsr,
    Asr,
    Neg,
    Mvn,
    Cmp,
    B,
    BEq,
    BNe,
    BLt,
    BLe,
    BGt,
    BGe,
    Bl,
    Ret,
    Ldr,
    Str,
    Ldp,
    Stp,
    Svc,
    Cset,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Operand {
    Register(Register),
    Immediate(i64),
    Label(String),
    MemBase(Register),
    MemOffset(Register, i32),
    MemPreIndex(Register, i32),
    MemPostIndex(Register, i32),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Register {
    X0,
    X1,
    X2,
    X3,
    X4,
    X5,
    X6,
    X7,
    X8,
    X9,
    X10,
    X11,
    X12,
    X13,
    X14,
    X15,
    X16,
    X17,
    X18,
    X19,
    X20,
    X21,
    X22,
    X23,
    X24,
    X25,
    X26,
    X27,
    X28,
    X29,
    X30,
    Sp,
    Xzr,
    W0,
    W1,
    W2,
    W3,
    W4,
    W5,
    W6,
    W7,
    W8,
    W9,
    W10,
    W11,
    W12,
    W13,
    W14,
    W15,
    W16,
    W17,
    W18,
    W19,
    W20,
    W21,
    W22,
    W23,
    W24,
    W25,
    W26,
    W27,
    W28,
    W29,
    W30,
    Wsp,
    Wzr,
}

impl Display for AArch64Element {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AArch64Element::Label(l) => write!(f, "{}:", l.name),
            AArch64Element::Instruction(i) => write!(f, "{}", i),
            AArch64Element::Directive(d) => write!(f, "{}", d),
            AArch64Element::Declaration(d) => write!(f, "{}", d),
        }
    }
}

impl Display for Directive {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Directive::Global(name) => write!(f, ".global {}", name),
            Directive::Text => write!(f, ".text"),
            Directive::Data => write!(f, ".data"),
            Directive::Align(n) => write!(f, ".align {}", n),
        }
    }
}

impl Display for Declaration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Declaration::Word(name, val) => write!(f, "{}: .word {}", name, val),
            Declaration::Byte(name, val) => write!(f, "{}: .byte {}", name, val),
        }
    }
}

impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let ops: Vec<String> = self.operands.iter().map(|o| o.to_string()).collect();
        if ops.is_empty() {
            write!(f, "{}", self.opcode)
        } else {
            write!(f, "{} {}", self.opcode, ops.join(", "))
        }
    }
}

impl Display for Opcode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Opcode::Mov => "mov",
            Opcode::Movz => "movz",
            Opcode::Movk => "movk",
            Opcode::Add => "add",
            Opcode::Sub => "sub",
            Opcode::Mul => "mul",
            Opcode::Sdiv => "sdiv",
            Opcode::Udiv => "udiv",
            Opcode::And => "and",
            Opcode::Orr => "orr",
            Opcode::Eor => "eor",
            Opcode::Lsl => "lsl",
            Opcode::Lsr => "lsr",
            Opcode::Asr => "asr",
            Opcode::Neg => "neg",
            Opcode::Mvn => "mvn",
            Opcode::Cmp => "cmp",
            Opcode::B => "b",
            Opcode::BEq => "b.eq",
            Opcode::BNe => "b.ne",
            Opcode::BLt => "b.lt",
            Opcode::BLe => "b.le",
            Opcode::BGt => "b.gt",
            Opcode::BGe => "b.ge",
            Opcode::Bl => "bl",
            Opcode::Ret => "ret",
            Opcode::Ldr => "ldr",
            Opcode::Str => "str",
            Opcode::Ldp => "ldp",
            Opcode::Stp => "stp",
            Opcode::Svc => "svc",
            Opcode::Cset => "cset",
        };
        write!(f, "{}", s)
    }
}

impl Display for Operand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Operand::Register(r) => write!(f, "{}", r),
            Operand::Immediate(v) => write!(f, "#{}", v),
            Operand::Label(l) => write!(f, "{}", l),
            Operand::MemBase(r) => write!(f, "[{}]", r),
            Operand::MemOffset(r, off) => {
                if *off == 0 {
                    write!(f, "[{}]", r)
                } else {
                    write!(f, "[{}, #{}]", r, off)
                }
            }
            Operand::MemPreIndex(r, off) => write!(f, "[{}, #{}]!", r, off),
            Operand::MemPostIndex(r, off) => write!(f, "[{}], #{}", r, off),
        }
    }
}

impl Display for Register {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Register::X0 => "x0",
            Register::X1 => "x1",
            Register::X2 => "x2",
            Register::X3 => "x3",
            Register::X4 => "x4",
            Register::X5 => "x5",
            Register::X6 => "x6",
            Register::X7 => "x7",
            Register::X8 => "x8",
            Register::X9 => "x9",
            Register::X10 => "x10",
            Register::X11 => "x11",
            Register::X12 => "x12",
            Register::X13 => "x13",
            Register::X14 => "x14",
            Register::X15 => "x15",
            Register::X16 => "x16",
            Register::X17 => "x17",
            Register::X18 => "x18",
            Register::X19 => "x19",
            Register::X20 => "x20",
            Register::X21 => "x21",
            Register::X22 => "x22",
            Register::X23 => "x23",
            Register::X24 => "x24",
            Register::X25 => "x25",
            Register::X26 => "x26",
            Register::X27 => "x27",
            Register::X28 => "x28",
            Register::X29 => "x29",
            Register::X30 => "x30",
            Register::Sp => "sp",
            Register::Xzr => "xzr",
            Register::W0 => "w0",
            Register::W1 => "w1",
            Register::W2 => "w2",
            Register::W3 => "w3",
            Register::W4 => "w4",
            Register::W5 => "w5",
            Register::W6 => "w6",
            Register::W7 => "w7",
            Register::W8 => "w8",
            Register::W9 => "w9",
            Register::W10 => "w10",
            Register::W11 => "w11",
            Register::W12 => "w12",
            Register::W13 => "w13",
            Register::W14 => "w14",
            Register::W15 => "w15",
            Register::W16 => "w16",
            Register::W17 => "w17",
            Register::W18 => "w18",
            Register::W19 => "w19",
            Register::W20 => "w20",
            Register::W21 => "w21",
            Register::W22 => "w22",
            Register::W23 => "w23",
            Register::W24 => "w24",
            Register::W25 => "w25",
            Register::W26 => "w26",
            Register::W27 => "w27",
            Register::W28 => "w28",
            Register::W29 => "w29",
            Register::W30 => "w30",
            Register::Wsp => "wsp",
            Register::Wzr => "wzr",
        };
        write!(f, "{}", s)
    }
}

pub const ARG_REGS_64: [Register; 8] = [
    Register::X0,
    Register::X1,
    Register::X2,
    Register::X3,
    Register::X4,
    Register::X5,
    Register::X6,
    Register::X7,
];

pub const ARG_REGS_32: [Register; 8] = [
    Register::W0,
    Register::W1,
    Register::W2,
    Register::W3,
    Register::W4,
    Register::W5,
    Register::W6,
    Register::W7,
];
