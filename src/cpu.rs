use std::ptr::NonNull;

pub struct Arm7TDMI {
    registers: [u32; 16],
    fiq_registers: [u32; 7],
    svc_registers: [u32; 2],
    abt_registers: [u32; 2],
    irq_registers: [u32; 2],
    und_registers: [u32; 2],
    cpsr: Psr,
    spsr: Option<NonNull<Psr>>,
    _spsr: [Psr; 5],
    mode: Mode,

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
    fn get_reg<const N: usize>(&mut self) -> &mut u32 {
        if N < 8 || N == 15 {
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
        }
    }

    #[rustfmt::skip]
    fn get_reg_rt(&mut self, n: u8) -> &mut u32 {
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

    fn get_sp(&mut self) -> &mut u32 {
        &mut self.registers[13]
    }

    fn get_lr(&mut self) -> &mut u32 {
        &mut self.registers[14]
    }

    fn get_pc(&mut self) -> &mut u32 {
        &mut self.registers[15]
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
struct Psr {
    signed: bool,
    zero: bool,
    carry: bool,
    overflow: bool,
    irq: bool,
    fiq: bool,
    state: bool,
    // Really a u4
    mode: u8,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum Mode {
    User,
    Fiq,
    Supervisor,
    Abort,
    Irq,
    Undefined,
}

enum Operand<'a> {
    Immediate(u32),
    LogShiftLImm(u32, u8),
    LogShiftRImm(u32, u8),
    RotateRImm(u32, u8),
    Register(&'a mut u32),
    LogShiftLReg(u32, u32),
    LogShiftRReg(u32, u32),
    ArithShiftR(u32, u32),
    RotateRReg(u32, u32),
    RotateRExt(u32),
}

impl<'a> Operand<'a> {
    fn get(self, cpu: &mut Arm7TDMI) -> u32 {
        match self {
            Operand::Immediate(v) => v,
            Operand::LogShiftLImm(v, s) => v << s,
            Operand::LogShiftRImm(v, s) => v >> s,
            Operand::RotateRImm(v, s) => v.rotate_right(s as u32),
            Operand::Register(r) => *r,
            Operand::LogShiftLReg(v, r) => v << r,
            Operand::LogShiftRReg(v, r) => v >> r,
            Operand::ArithShiftR(v, r) => (v as i32 >> r) as u32,
            Operand::RotateRReg(v, r) => v.rotate_right(r),
            Operand::RotateRExt(r) => {
                let c = &mut cpu.cpsr.carry;
                let one = r & 0x1;
                let r = ((*c as u32) << 31) | (r >> 1);
                *c = one == 1;
                r
            }
        }
    }
}

enum PsrArg {
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

enum Field {
    Flags,
    Status,
    Extension,
    Control,
}

// Instruction set implementation
/// The ARM7TDMI implements the ARMv4T instruction set. Notable features are the 16 bit Thumb
/// instruction set, three stage pipeline, and the lack of a 26 bit addressing mode.
impl Arm7TDMI {
    fn mov(&mut self, rd: Operand, op2: Operand) {
        if let Operand::Register(r) = rd {
            let val = op2.get(self);
            *r = val;
        } else {
            unsafe { std::hint::unreachable_unchecked() }
        }
    }
    fn mvn(&mut self, rd: Operand, op2: Operand) {
        if let Operand::Register(r) = rd {
            let val = op2.get(self);
            *r = !val;
        } else {
            unsafe { std::hint::unreachable_unchecked() }
        }
    }

    /// Move the contents of the CPSR or SPSR into a register.
    fn mrs(&mut self, rd: Operand, psr: PsrArg) {
        if let Operand::Register(r) = rd {
            let psr = if let PsrArg::Cpsr = psr {
                self.cpsr
            } else {
                // Safety: Calling MRS with SPSR in User mode is undefined. We can just crash
                // here.
                unsafe { *self.spsr.unwrap_unchecked().as_ref() }
            };
        } else {
            unsafe { std::hint::unreachable_unchecked() }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_operands() {
        let op = Operand::RotateRExt(0b10100101100011010011010001101001);
        let mut cpu = Arm7TDMI::new();
        cpu.cpsr.carry = false;
        assert_eq!(op.get(&mut cpu), 0b01010010110001101001101000110100);
        assert!(cpu.cpsr.carry);
    }
}
