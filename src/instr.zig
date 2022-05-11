const Instruction = struct {
    instr: union(enum) {
        // Logical ALU
        mov: struct {
            s: bool,
            rd: Register,
            op2: Op2,
        },
        mvn: struct {
            s: bool,
            rd: Register,
            op2: Op2,
        },
        orr: struct {
            s: bool,
            rd: Register,
            rn: Register,
            op2: Op2,
        },
        eor,
        and_,
        bic,
        tst,
        teq,

        // Arithmetic ALU
        add,
        adc,
        sub,
        sbc,
        rsb,
        rsc,
        cmp,
        cmn,

        // Multiply
        mul,
        mla,
        umull,
        umlal,
        smull,
        smlal,

        // Load/Store
        ldr,
        ldm,
        str,
        stm,
        swp,

        // Jump
        b,
        bl,
        bx,
        blx,
        mrs,
        msr,
        mwi,
        bkpt,
        undef,
        nop,
    },
    cond: Cond,
};

pub const Register = u4;

const Cond = enum(u8) {
    eq,
    ne,
    cs,
    cc,
    mi,
    pl,
    vs,
    vc,
    hi,
    ls,
    ge,
    lt,
    gt,
    le,
    al,
    nev,
};

const Op2 = union(enum) {
    register: struct { shift: u8, reg: u4 },
    imm: struct {
        rot: u4,
        imm: u8,
    },
};
