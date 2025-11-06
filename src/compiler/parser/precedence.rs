use std::ops::Add;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(u32)]
pub enum BindingPower {
    None = 0,
    Assignment = 1,    // = (right-associative)
    LogicalOr = 2,     // ||
    LogicalAnd = 3,    // &&
    BitwiseOr = 4,     // |
    BitwiseXor = 5,    // ^
    BitwiseAnd = 6,    // &
    Equality = 7,      // == !=
    Comparison = 8,    // < > <= >=
    Term = 9,          // + -
    Factor = 10,       // * / %
    Cast = 11,         // as (type cast)
    Unary = 12,        // Prefix - ! @ &
    Call = 13,         // () (function call)
    MemberAccess = 14, // . (field access)
    Primary = 15,      // Literals, identifiers
    More(u32),
}

impl From<u32> for BindingPower {
    fn from(val: u32) -> Self {
        match val {
            0 => BindingPower::None,
            1 => BindingPower::Assignment,
            2 => BindingPower::LogicalOr,
            3 => BindingPower::LogicalAnd,
            4 => BindingPower::BitwiseOr,
            5 => BindingPower::BitwiseXor,
            6 => BindingPower::BitwiseAnd,
            7 => BindingPower::Equality,
            8 => BindingPower::Comparison,
            9 => BindingPower::Term,
            10 => BindingPower::Factor,
            11 => BindingPower::Cast,
            12 => BindingPower::Unary,
            13 => BindingPower::Call,
            14 => BindingPower::MemberAccess,
            15 => BindingPower::Primary,
            _ => BindingPower::More(val),
        }
    }
}

impl BindingPower {
    pub fn val(&self) -> u32 {
        match self {
            BindingPower::None => 0,
            BindingPower::Assignment => 1,
            BindingPower::LogicalOr => 2,
            BindingPower::LogicalAnd => 3,
            BindingPower::BitwiseOr => 4,
            BindingPower::BitwiseXor => 5,
            BindingPower::BitwiseAnd => 6,
            BindingPower::Equality => 7,
            BindingPower::Comparison => 8,
            BindingPower::Term => 9,
            BindingPower::Factor => 10,
            BindingPower::Cast => 11,
            BindingPower::Unary => 12,
            BindingPower::Call => 13,
            BindingPower::MemberAccess => 14,
            BindingPower::Primary => 15,
            BindingPower::More(val) => *val,
        }
    }
}

impl Add for BindingPower {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        let sum_val = self.val() + rhs.val();
        BindingPower::from(sum_val)
    }
}
