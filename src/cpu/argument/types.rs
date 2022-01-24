use super::alu::AluOp2;
use crate::cpu::cpu::{Arm7TDMI, Psr};

pub type Register = u8;

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
    LogShiftLReg(Register, u8),
    LogShiftLImm(Register, u8),
    LogShiftRReg(Register, u8),
    LogShiftRImm(Register, u8),
    ArithShiftReg(Register, u8),
    ArithShiftImm(Register, u8),
    RotateRReg(Register, u8),
    RotateRRegImm(Register, u8),
    RotateRExt(Register),
}

impl Operand {
    pub fn get(&self, cpu: &mut Arm7TDMI, set_flag: bool) -> u32 {
        match *self {
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

    pub fn nonzero(&self) -> bool {
        // Rs != 00h
        match self {
            Operand::LogShiftLImm(_, 0) | Operand::LogShiftLReg(_, 0) => true,
            _ => false,
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
