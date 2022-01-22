use crate::cpu::cpu::{Arm7TDMI, Psr};

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

pub(super) enum ShiftType {
    LogShiftLeft,
    LogShiftRight,
    ArithShiftRight,
    RotateRight,
}

impl From<u8> for ShiftType {
    fn from(n: u8) -> Self {
        match n {
            0x0 => Self::LogShiftLeft,
            0x1 => Self::LogShiftRight,
            0x2 => Self::ArithShiftRight,
            0x3 => Self::RotateRight,
            _ => unsafe { std::hint::unreachable_unchecked() },
        }
    }
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

pub enum Condition {
    ZSet,
    ZClear,
    CSet,
    CClear,
    NSet,
    NClear,
    VSet,
    VClear,
    Hi,
    Ls,
    Ge,
    Lt,
    Gt,
    Le,
    Al,
    Ne,
}

impl From<u8> for Condition {
    fn from(v: u8) -> Self {
        match v {
            0x0 => Self::ZSet,
            0x1 => Self::ZClear,
            0x2 => Self::CSet,
            0x3 => Self::CClear,
            0x4 => Self::NSet,
            0x5 => Self::NClear,
            0x6 => Self::VSet,
            0x7 => Self::VClear,
            0x8 => Self::Hi,
            0x9 => Self::Ls,
            0xA => Self::Ge,
            0xB => Self::Lt,
            0xC => Self::Gt,
            0xD => Self::Le,
            0xE => Self::Al,
            0xF => Self::Ne,
            _ => unsafe { std::hint::unreachable_unchecked() },
        }
    }
}

pub enum Operand {
    // (value, shift * 2)
    RotateRImm(u8, u8),
    // (reg, shift)
    LogShiftLReg(u8, u8),
    LogShiftLImm(u8, u8),
    LogShiftRReg(u8, u8),
    LogShiftRImm(u8, u8),
    ArithShiftReg(u8, u8),
    ArithShiftImm(u8, u8),
    RotateRReg(u8, u8),
    RotateRRegImm(u8, u8),
    // (reg)
    RotateRExt(u8),
}

impl Operand {
    pub fn get(self, cpu: &mut Arm7TDMI, set_flag: bool) -> u32 {
        match self {
            Operand::RotateRImm(v, i) => (v as u32).rotate_right(i as u32 * 2),
            Operand::LogShiftLReg(v, n) => cpu.get_reg_rt(v) << cpu.get_reg_rt(n),
            Operand::LogShiftLImm(v, i) => cpu.get_reg_rt(v) << i,
            Operand::LogShiftRReg(v, n) => cpu.get_reg_rt(v) >> cpu.get_reg_rt(n),
            Operand::LogShiftRImm(v, i) => cpu.get_reg_rt(v) >> i,
            Operand::ArithShiftReg(v, n) => {
                if set_flag {
                    let carry = (cpu.get_reg_rt(v) >> (cpu.get_reg_rt(n) - 1)) & 0x01;
                    cpu.cpsr.set_carry(carry == 1);
                }
                ((cpu.get_reg_rt(v) as i32) >> cpu.get_reg_rt(n)) as u32
            }
            Operand::ArithShiftImm(v, i) => {
                if set_flag {
                    let carry = (cpu.get_reg_rt(v) >> (i - 1)) & 0x01;
                    cpu.cpsr.set_carry(carry == 1);
                }
                ((cpu.get_reg_rt(v) as i32) >> i) as u32
            }
            Operand::RotateRReg(v, n) => cpu
                .get_reg_rt(v)
                .rotate_right((cpu.get_reg_rt(n) & 0x0F) % 32),
            Operand::RotateRRegImm(v, i) => cpu.get_reg_rt(v).rotate_right(i as u32),
            Operand::RotateRExt(r) => {
                let reg = cpu.get_reg_rt(r);
                if set_flag {
                    cpu.cpsr.set_carry((reg & 0x01) == 1);
                }
                ((cpu.cpsr.carry() as u32) << 31) | (reg >> 1)
            }
        }
    }
}

impl From<AluOp2> for Operand {
    fn from(s: AluOp2) -> Self {
        match s {
            AluOp2::Register(r) => match r.shift_type {
                ShiftType::LogShiftLeft => {
                    if r.by_reg {
                        Operand::LogShiftLReg(r.rm, r.shift)
                    } else {
                        Operand::LogShiftLImm(r.rm, r.shift)
                    }
                }
                ShiftType::LogShiftRight => {
                    if r.by_reg {
                        Operand::LogShiftRReg(r.rm, r.shift)
                    } else {
                        Operand::LogShiftRImm(r.rm, r.shift)
                    }
                }
                ShiftType::ArithShiftRight => {
                    if r.by_reg {
                        Operand::ArithShiftReg(r.rm, r.shift)
                    } else {
                        Operand::ArithShiftImm(r.rm, r.shift)
                    }
                }
                ShiftType::RotateRight => {
                    if r.by_reg {
                        Operand::RotateRReg(r.rm, r.shift)
                    } else {
                        if r.shift != 0 {
                            Operand::RotateRRegImm(r.rm, r.shift)
                        } else {
                            Operand::RotateRExt(r.rm)
                        }
                    }
                }
            },
            AluOp2::Immediate(i) => Operand::RotateRImm(i.imm, i.shift),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum PsrArg {
    Cpsr,
    Spsr,
}

impl PsrArg {
    fn get(self, cpu: &mut Arm7TDMI) -> &mut Psr {
        match self {
            PsrArg::Cpsr => &mut cpu.cpsr,
            PsrArg::Spsr => unsafe { cpu.spsr.unwrap_unchecked().as_mut() },
        }
    }
}
