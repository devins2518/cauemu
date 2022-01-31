use super::types::{Condition, Register};

pub struct MulInstr {
    pub cond: Condition,
    pub s: bool,
    pub opcode: MulOpcode,
    pub rd: Register,
    pub rn: Register,
    pub rm: Register,
    pub rs: Register,
}

#[derive(Debug, PartialEq, Eq)]
pub enum MulOpcode {
    Mul,
    Mla,
    Umull,
    Umlal,
    Smull,
    Smlal,
}

impl From<u8> for MulOpcode {
    fn from(v: u8) -> Self {
        match v {
            0x0 => Self::Mul,
            0x1 => Self::Mla,
            0x4 => Self::Umull,
            0x5 => Self::Umlal,
            0x6 => Self::Smull,
            0x7 => Self::Smlal,
            _ => unsafe { std::hint::unreachable_unchecked() },
        }
    }
}
