use super::types::{Condition, Register};

pub struct BranchLinkInstr {
    pub cond: Condition,
    pub opcode: BranchOpcode,
    pub nn: i32,
}

pub struct BranchExchangeInstr {
    pub cond: Condition,
    pub rn: Register,
}

#[derive(Debug, PartialEq, Eq)]
pub enum BranchOpcode {
    B,
    Bl,
}

impl From<u8> for BranchOpcode {
    fn from(v: u8) -> Self {
        match v {
            0x0 => Self::B,
            0x1 => Self::Bl,
            _ => unsafe { std::hint::unreachable_unchecked() },
        }
    }
}
