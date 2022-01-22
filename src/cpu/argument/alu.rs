use super::types::{Condition, Operand, PsrArg, ShiftType};

pub(super) struct AluInstr {
    pub cond: Condition,
    pub immediate: bool,
    pub opcode: AluOpcode,
    pub set_conditions: bool,
    pub rn: u8,
    pub rd: u8,
    pub op2: AluOp2,
}

#[derive(Debug, PartialEq, Eq)]
pub(super) enum AluOpcode {
    And,
    Eor,
    Sub,
    Rsb,
    Add,
    Adc,
    Sbc,
    Rsc,
    Tst,
    Teq,
    Cmp,
    Cmn,
    Orr,
    Mov,
    Bic,
    Mvn,
}

impl From<u8> for AluOpcode {
    fn from(v: u8) -> Self {
        match v {
            0x0 => Self::And,
            0x1 => Self::Eor,
            0x2 => Self::Sub,
            0x3 => Self::Rsb,
            0x4 => Self::Add,
            0x5 => Self::Adc,
            0x6 => Self::Sbc,
            0x7 => Self::Rsc,
            0x8 => Self::Tst,
            0x9 => Self::Teq,
            0xA => Self::Cmp,
            0xB => Self::Cmn,
            0xC => Self::Orr,
            0xD => Self::Mov,
            0xE => Self::Bic,
            0xF => Self::Mvn,
            _ => unsafe { std::hint::unreachable_unchecked() },
        }
    }
}

pub(super) enum AluOp2 {
    Register(AluOp2Reg),
    Immediate(AluOp2Imm),
}

pub(super) struct AluOp2Reg {
    /// Either a shift amount or a register value used to shift.
    pub shift: u8,
    pub shift_type: ShiftType,
    pub by_reg: bool,
    pub rm: u8,
}

pub(super) struct AluOp2Imm {
    pub shift: u8,
    pub imm: u8,
}

/// Instruction that moves val into register.
pub struct MovInstr {
    pub cond: Condition,
    pub s: bool,
    pub register: u8,
    pub op2: Operand,
}

/// Instruction that moves PSR into register.
pub struct MrsInstr {
    pub cond: Condition,
    pub psr: PsrArg,
    pub register: u8,
}

/// Instruction that moves val into PSR_field.
pub struct MsrInstr {
    pub cond: Condition,
    pub psr: PsrArg,
    pub f: bool,
    pub s: bool,
    pub x: bool,
    pub c: bool,
    pub val: u32,
}
