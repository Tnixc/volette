use std::ops::Add;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(u32)]
pub enum BindingPower {
    None = 0,
    Assignment = 1,    // = (right-associative)
    LogicalOr = 2,     // ||
    LogicalAnd = 3,    // &&
    Equality = 4,      // == !=
    Comparison = 5,    // < > <= >=
    Term = 6,          // + -
    Factor = 7,        // * / %
    Unary = 8,         // Prefix - !
    Call = 9,          // () (function call)
    MemberAccess = 10, // . (field access)
    Primary = 11,      // Literals, identifiers
    More(u32),
}

impl From<u32> for BindingPower {
    fn from(val: u32) -> Self {
        match val {
            0 => BindingPower::None,
            1 => BindingPower::Assignment,
            2 => BindingPower::LogicalOr,
            3 => BindingPower::LogicalAnd,
            4 => BindingPower::Equality,
            5 => BindingPower::Comparison,
            6 => BindingPower::Term,
            7 => BindingPower::Factor,
            8 => BindingPower::Unary,
            9 => BindingPower::Call,
            10 => BindingPower::MemberAccess,
            11 => BindingPower::Primary,
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
            BindingPower::Equality => 4,
            BindingPower::Comparison => 5,
            BindingPower::Term => 6,
            BindingPower::Factor => 7,
            BindingPower::Unary => 8,
            BindingPower::Call => 9,
            BindingPower::MemberAccess => 10,
            BindingPower::Primary => 11,
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