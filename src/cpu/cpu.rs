use super::argument::{
    parser::{
        parse_alu, parse_branch_exchange, parse_branch_link, parse_mrs, parse_msr, parse_mul,
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
    fn get_lr(&self) -> u32 {
        self.get_reg::<14>()
    }

    #[inline]
    fn get_pc(&self) -> u32 {
        self.get_reg::<15>()
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

    #[inline]
    fn set_lr(&mut self, n: u32) {
        self.set_reg::<14>(n);
    }

    #[inline]
    fn set_pc(&mut self, n: u32) {
        self.set_reg::<15>(n);
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

    fn set_mode(&mut self, mode: Mode) {
        self.mode = mode;
    }

    fn log_flags(&mut self, shifted: u32, res: u32, nonzero: bool) {
        self.cpsr.set_zero(res == 0);
        self.cpsr.set_signed((res >> 31) == 1);
        if nonzero {
            self.cpsr.set_carry((shifted >> 31) == 1);
        }
    }

    fn arith_flags(&mut self, reg: u32, val: u32, res: u32) {
        self.cpsr.set_zero(res == 0);
        self.cpsr.set_signed((res >> 31) == 1);
        self.cpsr.set_carry(res < val || res < reg);
        self.cpsr.set_overflow(reg >> 31 != res >> 31)
    }

    fn mul_flags(&mut self, n: u32) {
        self.cpsr.set_zero(n == 0);
        self.cpsr.set_signed((n >> 31) == 1);
        self.cpsr.set_carry(false);
    }

    fn mul_long_flags(&mut self, n: u64) {
        self.cpsr.set_zero(n == 0);
        self.cpsr.set_signed((n >> 31) == 1);
        self.cpsr.set_carry(false);
        self.cpsr.set_overflow(false);
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
    /// Logical ALU

    /// Move the value of op2 into rd.
    fn mov(&mut self, instr: u32) {
        let instr = parse_alu(instr);
        let shifted = instr.op2.get(self, instr.s);
        let res = shifted;
        self.set_reg_rt(instr.rd, res);
        if instr.s && instr.rd != 15 {
            self.log_flags(shifted, res, instr.op2.nonzero());
        }
    }

    /// Move the negated contents of op2 into rd.
    fn mvn(&mut self, instr: u32) {
        let instr = parse_alu(instr);
        let shifted = instr.op2.get(self, instr.s);
        let res = !shifted;
        self.set_reg_rt(instr.rd, shifted);
        if instr.s && instr.rd != 15 {
            self.log_flags(shifted, res, instr.op2.nonzero());
        }
    }

    /// Set rd = rn | op2.
    fn orr(&mut self, instr: u32) {
        let instr = parse_alu(instr);
        let shifted = instr.op2.get(self, instr.s);
        let res = self.get_reg_rt(instr.rn) | shifted;
        self.set_reg_rt(instr.rd, res);
        if instr.s && instr.rd != 15 {
            self.log_flags(shifted, res, instr.op2.nonzero());
        }
    }

    /// Set rd = rn ^ op2.
    fn eor(&mut self, instr: u32) {
        let instr = parse_alu(instr);
        let shifted = instr.op2.get(self, instr.s);
        let res = self.get_reg_rt(instr.rn) ^ shifted;
        self.set_reg_rt(instr.rd, res);
        if instr.s && instr.rd != 15 {
            self.log_flags(shifted, res, instr.op2.nonzero());
        }
    }

    /// Set rd = rn & op2.
    fn and(&mut self, instr: u32) {
        let instr = parse_alu(instr);
        let shifted = instr.op2.get(self, instr.s);
        let res = self.get_reg_rt(instr.rn) & shifted;
        self.set_reg_rt(instr.rd, res);
        if instr.s && instr.rd != 15 {
            self.log_flags(shifted, res, instr.op2.nonzero());
        }
    }

    /// Set rd = rn & !op2.
    fn bic(&mut self, instr: u32) {
        let instr = parse_alu(instr);
        let shifted = instr.op2.get(self, instr.s);
        let res = self.get_reg_rt(instr.rn) & !shifted;
        self.set_reg_rt(instr.rd, res);
        if instr.s && instr.rd != 15 {
            self.log_flags(shifted, res, instr.op2.nonzero());
        }
    }

    /// Test rn & op2
    fn tst(&mut self, instr: u32) {
        let instr = parse_alu(instr);
        let shifted = instr.op2.get(self, true);
        let res = self.get_reg_rt(instr.rn) & shifted;
        if instr.s && instr.rd != 15 {
            self.log_flags(shifted, res, instr.op2.nonzero());
        }
    }

    /// Test rn ^ op2
    fn teq(&mut self, instr: u32) {
        let instr = parse_alu(instr);
        let shifted = instr.op2.get(self, true);
        let res = self.get_reg_rt(instr.rn) ^ shifted;
        if instr.s && instr.rd != 15 {
            self.log_flags(shifted, res, instr.op2.nonzero());
        }
    }

    /// Arithmetic ALU

    /// Set rd = rn + op2
    fn add(&mut self, instr: u32) {
        let instr = parse_alu(instr);
        let reg = self.get_reg_rt(instr.rn);
        let val = instr.op2.get(self, instr.s);
        let res = reg + val;
        self.set_reg_rt(instr.rd, res);
        if instr.s && instr.rd != 15 {
            self.arith_flags(reg, val, res);
        }
    }

    /// Set rd = rn + op2 + carry
    fn adc(&mut self, instr: u32) {
        let instr = parse_alu(instr);
        let reg = self.get_reg_rt(instr.rn);
        let val = instr.op2.get(self, instr.s);
        let res = self.get_reg_rt(instr.rn) + val + u32::from(self.cpsr.carry());
        self.set_reg_rt(instr.rd, res);
        if instr.s && instr.rd != 15 {
            self.arith_flags(reg, val, res);
        }
    }

    /// Set rd = rn - op2
    fn sub(&mut self, instr: u32) {
        let instr = parse_alu(instr);
        let reg = self.get_reg_rt(instr.rn);
        let val = instr.op2.get(self, instr.s);
        let res = self.get_reg_rt(instr.rn) - val;
        self.set_reg_rt(instr.rd, res);
        if instr.s && instr.rd != 15 {
            self.arith_flags(reg, val, res);
        }
    }

    /// Set rd = rn - op2 + carry - 1
    fn sbc(&mut self, instr: u32) {
        let instr = parse_alu(instr);
        let reg = self.get_reg_rt(instr.rn);
        let val = instr.op2.get(self, instr.s);
        let res = (self.get_reg_rt(instr.rn) - val) + (u32::from(self.cpsr.carry()) - 1);
        self.set_reg_rt(instr.rd, res);
        if instr.s && instr.rd != 15 {
            self.arith_flags(reg, val, res);
        }
    }

    /// Set rd = op2 - rn
    fn rsb(&mut self, instr: u32) {
        let instr = parse_alu(instr);
        let reg = self.get_reg_rt(instr.rn);
        let val = instr.op2.get(self, instr.s);
        let res = val - self.get_reg_rt(instr.rn);
        self.set_reg_rt(instr.rd, res);
        if instr.s && instr.rd != 15 {
            self.arith_flags(reg, val, res);
        }
    }

    /// Set rd = op2 - rn + carry - 1
    fn rsc(&mut self, instr: u32) {
        let instr = parse_alu(instr);
        let reg = self.get_reg_rt(instr.rn);
        let val = instr.op2.get(self, instr.s);
        let res = (val - self.get_reg_rt(instr.rn)) + (u32::from(self.cpsr.carry()) - 1);
        self.set_reg_rt(instr.rd, res);
        if instr.s && instr.rd != 15 {
            self.arith_flags(reg, val, res);
        }
    }

    /// Test rn - op2
    fn cmp(&mut self, instr: u32) {
        let instr = parse_alu(instr);
        let reg = self.get_reg_rt(instr.rn);
        let val = instr.op2.get(self, instr.s);
        let res = self.get_reg_rt(instr.rn) - val;
        if instr.s && instr.rd != 15 {
            self.arith_flags(reg, val, res);
        }
    }

    /// Test rn + op2
    fn cmn(&mut self, instr: u32) {
        let instr = parse_alu(instr);
        let reg = self.get_reg_rt(instr.rn);
        let val = instr.op2.get(self, instr.s);
        let res = reg + val;
        if instr.s && instr.rd != 15 {
            self.arith_flags(reg, val, res);
        }
    }

    /// Multiply

    /// Multiply unsigned rm and rs, keeping the lowest 32 bits.
    fn mul(&mut self, instr: u32) {
        let instr = parse_mul(instr);
        debug_assert!(instr.rd != instr.rm);
        debug_assert!(instr.rd != 15 || instr.rn != 15 || instr.rs != 15 || instr.rm != 15);
        let n: u32 = (((self.get_reg_rt(instr.rm) as u64) * (self.get_reg_rt(instr.rs) as u64))
            & 0xFFFFFFFF) as u32;
        self.set_reg_rt(instr.rd, n);
        if instr.s {
            self.mul_flags(n);
        }
    }

    /// Multiply unsigned rm and rs, add rn, and keep lowest 32 bits.
    fn mla(&mut self, instr: u32) {
        let instr = parse_mul(instr);
        debug_assert!(instr.rd != instr.rm);
        debug_assert!(instr.rd != 15 || instr.rn != 15 || instr.rs != 15 || instr.rm != 15);
        let n: u32 = (((self.get_reg_rt(instr.rm) as u64) * (self.get_reg_rt(instr.rs) as u64))
            & 0xFFFFFFFF) as u32
            + self.get_reg_rt(instr.rn);
        self.set_reg_rt(instr.rd, n);
        if instr.s {
            self.mul_flags(n);
        }
    }

    /// Multiply unsigned rm and rs, add rn, and put the 64 bit result into 2 registers.
    fn umull(&mut self, instr: u32) {
        let instr = parse_mul(instr);
        debug_assert!(instr.rd != instr.rm);
        debug_assert!(instr.rd != 15 || instr.rn != 15 || instr.rs != 15 || instr.rm != 15);
        let n: u64 = (self.get_reg_rt(instr.rm) as u64) * (self.get_reg_rt(instr.rs) as u64);
        self.set_reg_rt(instr.rd, (n >> 32) as u32);
        self.set_reg_rt(instr.rn, (n & 0xFFFFFFFF) as u32);
        if instr.s {
            self.mul_long_flags(n);
        }
    }

    /// Multiply unsigned rm and rs, add rn, and put the 64 bit result into 2 registers.
    fn umlal(&mut self, instr: u32) {
        let instr = parse_mul(instr);
        debug_assert!(instr.rd != instr.rm);
        debug_assert!(instr.rd != 15 || instr.rn != 15 || instr.rs != 15 || instr.rm != 15);
        let n: u64 = (self.get_reg_rt(instr.rm) as u64) * (self.get_reg_rt(instr.rs) as u64)
            + ((self.get_reg_rt(instr.rd) as u64) << 32 | self.get_reg_rt(instr.rn) as u64);
        self.set_reg_rt(instr.rd, (n >> 32) as u32);
        self.set_reg_rt(instr.rn, (n & 0xFFFFFFFF) as u32);
        if instr.s {
            self.mul_long_flags(n);
        }
    }

    /// Multiply signed rm and rs, add rn, and put the 64 bit result into 2 registers.
    fn smull(&mut self, instr: u32) {
        let instr = parse_mul(instr);
        debug_assert!(instr.rd != instr.rm);
        debug_assert!(instr.rd != 15 || instr.rn != 15 || instr.rs != 15 || instr.rm != 15);
        let n: i64 = (self.get_reg_rt(instr.rm) as i64) * (self.get_reg_rt(instr.rs) as i64);
        self.set_reg_rt(instr.rd, (n >> 32) as u32);
        self.set_reg_rt(instr.rn, (n & 0xFFFFFFFF) as u32);
        if instr.s {
            self.mul_long_flags(n as u64);
        }
    }

    /// Multiply signed rm and rs, add rn, and put the 64 bit result into 2 registers.
    fn smlal(&mut self, instr: u32) {
        let instr = parse_mul(instr);
        debug_assert!(instr.rd != instr.rm);
        debug_assert!(instr.rd != 15 || instr.rn != 15 || instr.rs != 15 || instr.rm != 15);
        let n: i64 = (self.get_reg_rt(instr.rm) as i64) * (self.get_reg_rt(instr.rs) as i64)
            + ((self.get_reg_rt(instr.rd) as i64) << 32 | self.get_reg_rt(instr.rn) as i64);
        self.set_reg_rt(instr.rd, (n >> 32) as u32);
        self.set_reg_rt(instr.rn, (n & 0xFFFFFFFF) as u32);
        if instr.s {
            self.mul_long_flags(n as u64);
        }
    }

    /// Branch and Link

    fn b(&mut self, instr: u32) {
        let instr = parse_branch_link(instr);
        self.set_pc(self.get_pc() + 8 + ((instr.nn * 4) as u32))
    }

    fn bl(&mut self, instr: u32) {
        let instr = parse_branch_link(instr);
        self.set_lr(self.get_pc() + 4);
        self.set_pc(self.get_pc() + 8 + ((instr.nn * 4) as u32));
    }

    fn bx(&mut self, instr: u32) {
        let instr = parse_branch_exchange(instr);
        let reg = self.get_reg_rt(instr.rn);
        self.cpsr.set_arm(!(reg & 0x1 == 0x1));
        self.set_pc(reg & 0xFFFFFFFE);
    }

    /// Software Interrupt

    fn swi(&mut self, instr: u32) {
        let bytes = instr.to_le_bytes();
        unimplemented!()
    }

    /// PSR Transfer

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
