use super::argument::{
    parser::{
        parse_and, parse_bic, parse_mov, parse_mrs, parse_msr, parse_orr, parse_teq, parse_tst,
    },
    types::PsrArg,
};
use modular_bitfield::prelude::*;
use std::ptr::NonNull;

pub struct Arm7TDMI {
    pub(super) registers: [u32; 16],
    pub(super) fiq_registers: [u32; 7],
    pub(super) svc_registers: [u32; 2],
    pub(super) abt_registers: [u32; 2],
    pub(super) irq_registers: [u32; 2],
    pub(super) und_registers: [u32; 2],
    pub(super) cpsr: Psr,
    pub(super) spsr: Option<NonNull<Psr>>,
    pub(super) _spsr: [Psr; 5],
    pub(super) mode: Mode,

    clocks: usize,
}

// Helper methods
impl Arm7TDMI {
    pub fn new() -> Self {
        Self {
            registers: [0x0; 16],
            fiq_registers: [0x0; 7],
            svc_registers: [0x0; 2],
            abt_registers: [0x0; 2],
            irq_registers: [0x0; 2],
            und_registers: [0x0; 2],
            cpsr: Psr::default(),
            spsr: None,
            _spsr: [Psr::default(); 5],
            mode: Mode::User,

            clocks: 0,
        }
    }

    pub fn clock(&mut self) {
        unimplemented!()
    }

    #[inline]
    fn get_reg<const N: usize>(&self) -> u32 {
        if N < 8 || N == 15 {
            self.registers[N]
        } else {
            match self.mode {
                Mode::User => self.registers[N],
                Mode::Fiq => self.fiq_registers[N - 7],
                Mode::Supervisor if N > 13 => self.svc_registers[N - 13],
                Mode::Abort if N > 13 => self.abt_registers[N - 13],
                Mode::Irq if N > 13 => self.irq_registers[N - 13],
                Mode::Undefined if N > 13 => self.und_registers[N - 13],
                _ => self.registers[N],
            }
        }
    }

    #[inline]
    fn set_reg<const N: usize>(&mut self, n: u32) {
        let reg = if N < 8 || N == 15 {
            &mut self.registers[N]
        } else {
            match self.mode {
                Mode::User => &mut self.registers[N],
                Mode::Fiq => &mut self.fiq_registers[N - 7],
                Mode::Supervisor if N > 13 => &mut self.svc_registers[N - 13],
                Mode::Abort if N > 13 => &mut self.abt_registers[N - 13],
                Mode::Irq if N > 13 => &mut self.irq_registers[N - 13],
                Mode::Undefined if N > 13 => &mut self.und_registers[N - 13],
                _ => &mut self.registers[N],
            }
        };
        *reg = n;
    }

    #[rustfmt::skip]
    pub(super) fn get_reg_rt(&self, n: u8) -> u32 {
        if n == 0 { self.get_reg::<0>() } else
        if n == 1 { self.get_reg::<1>() } else
        if n == 2 { self.get_reg::<2>() } else
        if n == 3 { self.get_reg::<3>() } else
        if n == 4 { self.get_reg::<4>() } else
        if n == 5 { self.get_reg::<5>() } else
        if n == 6 { self.get_reg::<6>() } else
        if n == 7 { self.get_reg::<7>() } else
        if n == 8 { self.get_reg::<8>() } else
        if n == 9 { self.get_reg::<9>() } else
        if n == 10 { self.get_reg::<10>() } else
        if n == 11 { self.get_reg::<11>() } else
        if n == 12 { self.get_reg::<12>() } else
        if n == 13 { self.get_reg::<13>() } else
        if n == 14 { self.get_reg::<14>() } else
        if n == 15 { self.get_reg::<15>() } else {
            unsafe{ std::hint::unreachable_unchecked() }
        }
    }

    #[rustfmt::skip]
    pub(super) fn set_reg_rt(&mut self, n: u8, v: u32)  {
        if n == 0 { self.set_reg::<0>(v) } else
        if n == 1 { self.set_reg::<1>(v) } else
        if n == 2 { self.set_reg::<2>(v) } else
        if n == 3 { self.set_reg::<3>(v) } else
        if n == 4 { self.set_reg::<4>(v) } else
        if n == 5 { self.set_reg::<5>(v) } else
        if n == 6 { self.set_reg::<6>(v) } else
        if n == 7 { self.set_reg::<7>(v) } else
        if n == 8 { self.set_reg::<8>(v) } else
        if n == 9 { self.set_reg::<9>(v) } else
        if n == 10 { self.set_reg::<10>(v) } else
        if n == 11 { self.set_reg::<11>(v) } else
        if n == 12 { self.set_reg::<12>(v) } else
        if n == 13 { self.set_reg::<13>(v) } else
        if n == 14 { self.set_reg::<14>(v) } else
        if n == 15 { self.set_reg::<15>(v) } else {
            unsafe{ std::hint::unreachable_unchecked() }
        }
    }

    fn get_psr(&self, psr: PsrArg) -> Psr {
        match psr {
            PsrArg::Cpsr => self.cpsr,
            PsrArg::Spsr => unsafe { *self.spsr.unwrap().as_ref() },
        }
    }

    fn set_psr(&mut self, psr: Psr, which: PsrArg) {
        match which {
            PsrArg::Cpsr => self.cpsr = psr,
            PsrArg::Spsr => unsafe { *self.spsr.unwrap().as_mut() = psr },
        }
    }
}

#[bitfield]
#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
pub(super) struct Psr {
    pub signed: bool,
    pub zero: bool,
    pub carry: bool,
    pub overflow: bool,
    #[skip]
    __: B21,
    pub irq: bool,
    pub fiq: bool,
    pub arm: bool,
    pub mode: B4,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(super) enum Mode {
    User,
    Fiq,
    Supervisor,
    Abort,
    Irq,
    Undefined,
}

// Instruction set implementation
/// The ARM7TDMI implements the ARMv4T instruction set. Notable features are the 16 bit Thumb
/// instruction set, three stage pipeline, and the lack of a 26 bit addressing mode.
impl Arm7TDMI {
    /// Move the value of op2 into rd.
    fn mov(&mut self, instr: u32) {
        let instr = parse_mov(instr, self);
        let val = instr.op2.get(self, instr.s);
        self.set_reg_rt(instr.register, val);
    }

    /// Move the negated contents of op2 into rd.
    fn mvn(&mut self, instr: u32) {
        let instr = parse_mov(instr, self);
        let val = instr.op2.get(self, instr.s);
        self.set_reg_rt(instr.register, !val);
    }

    fn orr(&mut self, instr: u32) {
        let instr = parse_orr(instr);
        let val = instr.op2.get(self, instr.s);
        self.set_reg_rt(instr.rd, self.get_reg_rt(instr.rn) | val);
    }

    fn eor(&mut self, instr: u32) {
        let instr = parse_orr(instr);
        let val = instr.op2.get(self, instr.s);
        self.set_reg_rt(instr.rd, self.get_reg_rt(instr.rn) ^ val);
    }

    fn and(&mut self, instr: u32) {
        let instr = parse_and(instr);
        let val = instr.op2.get(self, instr.s);
        self.set_reg_rt(instr.rd, self.get_reg_rt(instr.rn) & val);
    }

    fn bic(&mut self, instr: u32) {
        let instr = parse_bic(instr);
        let val = instr.op2.get(self, instr.s);
        self.set_reg_rt(instr.rd, self.get_reg_rt(instr.rn) & !val);
    }

    fn tst(&mut self, instr: u32) {
        let instr = parse_tst(instr);
        let val = instr.op2.get(self, true);
        let val = self.get_reg_rt(instr.rn) & val;
        self.cpsr.set_zero(val == 0);
        self.cpsr.set_signed((val >> 31) == 1);
    }

    fn teq(&mut self, instr: u32) {
        let instr = parse_teq(instr);
        let val = instr.op2.get(self, true);
        let val = self.get_reg_rt(instr.rn) == val;
        self.cpsr.set_zero(val);
        self.cpsr.set_signed(val);
    }

    /// Move the contents of the CPSR or SPSR into a register.
    fn mrs(&mut self, instr: u32) {
        let instr = parse_mrs(instr);
        self.set_reg_rt(
            instr.register,
            u32::from_le_bytes(self.get_psr(instr.psr).into_bytes()),
        );
    }

    /// Move the contents of a register to a PSR field
    fn msr(&mut self, instr: u32) {
        let instr = parse_msr(instr, self);
        let mut psr = self.get_psr(instr.psr).into_bytes();
        if instr.f {
            psr[0] = ((instr.val & 0xFF000000) >> 24) as u8;
        }
        if instr.s {
            psr[1] = ((instr.val & 0x00FF0000) >> 16) as u8;
        }
        if instr.x {
            psr[2] = ((instr.val & 0x0000FF00) >> 8) as u8;
        }
        if instr.c {
            psr[3] = (instr.val & 0x000000FF) as u8;
        }
        self.set_psr(Psr::from_bytes(psr), instr.psr);
    }
}
