use crate::cpu::argument::types::{
    AluOp2, AluOp2Imm, AluOp2Reg, AluOpcode, Condition, Operand, PsrArg, ShiftType,
};

use super::{
    super::Arm7TDMI,
    types::{AluInstr, MovInstr, MrsInstr, MsrInstr},
};

pub fn parse_mov(instr: u32, cpu: &Arm7TDMI) -> MovInstr {
    let alu = parse_alu(instr);
    debug_assert_eq!(alu.opcode, AluOpcode::Mov);
    debug_assert_eq!(alu.rn, 0x0);
    let op2 = Operand::from(alu.op2);
    MovInstr {
        cond: alu.cond,
        s: alu.set_conditions,
        register: alu.rd,
        op2,
    }
}

pub fn parse_mrs(instr: u32) -> MrsInstr {
    let bytes = instr.to_le_bytes();
    let cond = Condition::from(bytes[0] >> 4);
    debug_assert_eq!(bytes[0] & 0x0C, 0x00);
    debug_assert_eq!((bytes[0] & 0x02) >> 1, 0x00);
    debug_assert_eq!(((bytes[0] & 0x01) << 1) | ((bytes[1] & 0x80) >> 1), 0x10);
    let psr = if (bytes[1] & 0x40) >> 2 == 1 {
        PsrArg::Spsr
    } else {
        PsrArg::Cpsr
    };
    debug_assert_eq!(bytes[1] & 0x2F, 0x2F);
    let register = bytes[3] & 0xF0;
    debug_assert_eq!(bytes[3] & 0x0F, 0x00);
    debug_assert_eq!(bytes[4], 0x00);

    MrsInstr {
        cond,
        register,
        psr,
    }
}

pub fn parse_msr(instr: u32, cpu: &Arm7TDMI) -> MsrInstr {
    let bytes = instr.to_le_bytes();
    let cond = Condition::from(bytes[0] >> 4);
    debug_assert_eq!(bytes[0] & 0x0C, 0x00);
    let immediate = ((bytes[0] & 0x02) >> 1) == 1;
    debug_assert_eq!(((bytes[0] & 0x01) << 1) | ((bytes[1] & 0x80) >> 1), 0x10);
    let psr = if (bytes[1] & 0x40) >> 2 == 1 {
        PsrArg::Spsr
    } else {
        PsrArg::Cpsr
    };
    debug_assert_eq!((bytes[1] & 0x20) >> 5, 0x01);
    debug_assert_eq!((bytes[1] & 0x10) >> 4, 0x00);
    let f = ((bytes[3] & 0x08) >> 3) == 1;
    let s = ((bytes[3] & 0x04) >> 2) == 1;
    let x = ((bytes[3] & 0x02) >> 1) == 1;
    let c = (bytes[3] & 0x01) == 1;
    debug_assert_eq!(bytes[2] >> 4, 0x0F);
    let val = if immediate {
        (bytes[4] as u32).rotate_right((bytes[3] & 0x0F) as u32 * 2)
    } else {
        debug_assert_eq!(((bytes[3] & 0x0F) << 4) | ((bytes[4] & 0xF0) >> 4), 0x00);
        cpu.get_reg_rt(bytes[4] & 0x0F)
    };

    MsrInstr {
        cond,
        psr,
        f,
        s,
        x,
        c,
        val,
    }
}

fn parse_alu(instr: u32) -> AluInstr {
    let bytes = instr.to_le_bytes();
    let cond = Condition::from(bytes[0] >> 4);
    debug_assert_eq!(bytes[0] & 0x0C, 0x00);
    let immediate = ((bytes[0] & 0x02) >> 1) == 1;
    let opcode = AluOpcode::from(((bytes[1] & 0xE0) >> 5) | ((bytes[0] & 0x01) << 3));
    let set_conditions = ((bytes[1] & 0x10) >> 4) == 1;
    let rn = bytes[1] & 0x0F;
    let rd = (bytes[2] & 0xF0) >> 4;
    let op2 = if immediate {
        AluOp2::Immediate(AluOp2Imm {
            shift: bytes[3] & 0x0F,
            imm: bytes[4],
        })
    } else {
        let by_reg = ((bytes[4] & 0x80) >> 3) == 1;
        let shift = if by_reg {
            debug_assert_eq!((bytes[4] & 0x80) >> 7, 0x0);
            bytes[3] & 0x0F
        } else {
            ((bytes[3] & 0x0F) << 1) | ((bytes[4] & 0x80) >> 7)
        };
        let shift_type = ShiftType::from((bytes[4] & 0x60) >> 5);
        AluOp2::Register(AluOp2Reg {
            shift,
            shift_type,
            by_reg,
            rm: bytes[4] & 0x0F,
        })
    };
    AluInstr {
        cond,
        immediate,
        opcode,
        set_conditions,
        rn,
        rd,
        op2,
    }
}
