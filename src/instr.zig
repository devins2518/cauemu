const std = @import("std");
const utils = @import("utils.zig");
const Field = utils.Field;

const Instruction = struct {
    instr: union(enum) {
        // Logical ALU
        mov: AluInstr,
        mvn: AluInstr,
        orr: AluInstr,
        eor: AluInstr,
        and_: AluInstr,
        bic: AluInstr,
        tst: AluInstr,
        teq: AluInstr,

        // Arithmetic ALU
        add: AluInstr,
        adc: AluInstr,
        sub: AluInstr,
        sbc: AluInstr,
        rsb: AluInstr,
        rsc: AluInstr,
        cmp: AluInstr,
        cmn: AluInstr,

        // Multiply
        mul: MulInstr,
        mla: MulInstr,
        umull: MulInstr,
        umlal: MulInstr,
        smull: MulInstr,
        smlal: MulInstr,

        // Load/Store
        ldr: SDTransferInstr,
        ldm: BDTransferInstr,
        str: SDTransferInstr,
        stm: BDTransferInstr,
        swp: SDSwapInstr,

        // Jump
        b: BranchInstr,
        bl: BranchInstr,
        bx: BranchExInstr,
        mrs: PSRTransferInstr,
        msr: PSRTransferInstr,
        swi,
        undef,
        nop,
    },
    cond: Cond,
};

pub const Register = u4;

const Cond = enum(u4) {
    eq = 0x0,
    ne = 0x1,
    cs = 0x2,
    cc = 0x3,
    mi = 0x4,
    pl = 0x5,
    vs = 0x6,
    vc = 0x7,
    hi = 0x8,
    ls = 0x9,
    ge = 0xA,
    lt = 0xB,
    gt = 0xC,
    le = 0xD,
    al = 0xE,
    nev = 0xF,
};

const Op2 = union(enum) {
    reg: struct {
        shift_by: union(enum) {
            imm: u5,
            reg: Register,
        },
        shift_type: enum(u2) {
            lsl = 0x0,
            lsr = 0x1,
            asr = 0x2,
            ror = 0x3,
        },
        reg: u4,
    },
    imm: struct {
        rot: u4,
        imm: u8,
    },
};
fn parseOp2(op: u32) Op2 {
    const imm = (op & 0x02000000) >> 25 == 1;
    if (imm) {
        return .{ .imm = .{
            .rot = @truncate(u4, (op & 0x00000F00) >> 8),
            .imm = @truncate(u8, op & 0x000000FF),
        } };
    } else {
        return .{
            .reg = .{
                .shift_by = blk: {
                    if (op & 0x00000010 == 1) {
                        break :blk .{ .imm = @truncate(u5, (op & 0x00000F80) >> 7) };
                    } else {
                        std.debug.assert(op & 0x00000F00 == 0);
                        break :blk .{ .reg = @truncate(u4, (op & 0x00000F00) >> 8) };
                    }
                },
                .shift_type = @intToEnum(Field(Field(Op2, .reg), .shift_type), (op & 0x00000060) >> 5),
                .reg = @truncate(u4, op & 0x0000000F),
            },
        };
    }
}

pub fn parseOpcode(op: u32) Instruction {
    return .{
        .instr = switch (((op & 0x0FF00000) >> 16) | ((op & 0xF0) >> 4)) {
            // Logical ALU
            // zig fmt: off
            0x1A0...0x1A8, 0x1AA, 0x1AC, 0x1AE,
            0x1B0...0x1B8, 0x1BA, 0x1BC, 0x1BE,
            0x3A0...0x3BF,
            => .{ .mov = AluInstr.parseAlu(op, .mov) },
            0x1E0...0x1E8, 0x1EA, 0x1EC, 0x1EE,
            0x1F0...0x1F8, 0x1FA, 0x1FC, 0x1FE,
            0x3E0...0x3FF,
            => .{ .mvn = AluInstr.parseAlu(op, .mvn) },
            0x180...0x188, 0x18A, 0x18C, 0x18E,
            0x190...0x198, 0x19A, 0x19C, 0x19E,
            0x380...0x39F,
            => .{ .orr = AluInstr.parseAlu(op, .orr) },
            0x020...0x028, 0x02A, 0x02C, 0x02E,
            0x030...0x038, 0x03A, 0x03C, 0x03E,
            0x220...0x23F,
            => .{ .eor = AluInstr.parseAlu(op, .eor) },
            0x000...0x008, 0x00A, 0x00C, 0x00E,
            0x010...0x018, 0x01A, 0x01C, 0x01E,
            0x200...0x21F,
            => .{ .and_ = AluInstr.parseAlu(op, .and_) },
            0x1C0...0x1C8, 0x1CA, 0x1CC, 0x1CE,
            0x1D0...0x1D8, 0x1DA, 0x1DC, 0x1DE,
            0x3C0...0x3DF,
            => .{ .bic = AluInstr.parseAlu(op, .bic) },
            0x110...0x118, 0x11A, 0x11C, 0x11E,
            0x310...0x31F,
            => .{ .tst = AluInstr.parseAlu(op, .tst) },
            0x130...0x138, 0x13A, 0x13C, 0x13E,
            0x330...0x33F,
            => .{ .teq = AluInstr.parseAlu(op, .teq) },
            // zig fmt: on

            // Arithmetic ALU
            // zig fmt: off
            0x080...0x088, 0x08A, 0x08C, 0x08E,
            0x090...0x098, 0x09A, 0x09C, 0x09E,
            0x280...0x29F,
            => .{ .add = AluInstr.parseAlu(op, .add) },
            0x0A0...0x0A8, 0x0AA, 0x0AC, 0x0AE,
            0x0B0...0x0B8, 0x0BA, 0x0BC, 0x0BE,
            0x2A0...0x2BF,
            => .{ .adc = AluInstr.parseAlu(op, .adc) },
            0x040...0x048, 0x04A, 0x04C, 0x04E,
            0x050...0x058, 0x05A, 0x05C, 0x05E,
            0x240...0x25F,
            => .{ .sub = AluInstr.parseAlu(op, .sub) },
            0x0C0...0x0C8, 0x0CA, 0x0CC, 0x0CE,
            0x0D0...0x0D8, 0x0DA, 0x0DC, 0x0DE,
            0x2C0...0x2DF,
            => .{ .sbc = AluInstr.parseAlu(op, .sbc) },
            0x060...0x068, 0x06A, 0x06C, 0x06E,
            0x070...0x078, 0x07A, 0x07C, 0x07E,
            0x260...0x27F,
            => .{ .rsb = AluInstr.parseAlu(op, .rsb) },
            0x0E0...0x0E8, 0x0EA, 0x0EC, 0x0EE,
            0x0F0...0x0F8, 0x0FA, 0x0FC, 0x0FE,
            0x2E0...0x2FF,
            => .{ .rsc = AluInstr.parseAlu(op, .rsc) },
            0x150...0x158, 0x15A, 0x15C, 0x15E,
            0x350...0x35F,
            => .{ .cmp = AluInstr.parseAlu(op, .cmp) },
            0x170...0x178, 0x17A, 0x17C, 0x17E,
            0x370...0x37F,
            => .{ .cmn = AluInstr.parseAlu(op, .cmn) },
            // zig fmt: on

            //Multiply
            // zig fmt: off
            0x009, 0x019,
            => .{ .mul = MulInstr.parseMul(op, .mul) },
            0x029, 0x039,
            => .{ .mla = MulInstr.parseMul(op, .mla) },
            0x089, 0x099,
            => .{ .umull = MulInstr.parseMul(op, .umull) },
            0x0A9, 0x0B9,
            => .{ .umlal = MulInstr.parseMul(op, .umlal) },
            0x0C9, 0x0D9,
            => .{ .smull = MulInstr.parseMul(op, .smull) },
            0x0E9, 0x0F9,
            => .{ .smlal = MulInstr.parseMul(op, .smlal) },
            // zig fmt: on

            // Load/Store
            // zig fmt: off
            0x00D, 0x02D, 0x04D, 0x06D, 0x08D, 0x10D, 0x12D, 0x14D, 0x16D, 0x18D,
            0x01B, 0x01D, 0x01F, 0x03B, 0x03D, 0x03F, 0x05B, 0x05D, 0x05F, 0x07B,
            0x07D, 0x07F, 0x09B, 0x09D, 0x09F, 0x11B, 0x11D, 0x11F, 0x13B, 0x13D,
            0x13F, 0x15B, 0x15D, 0x15F, 0x17B, 0x17D, 0x17F, 0x19B, 0x19D, 0x19F,
            0x410...0x41F, 0x430...0x43F, 0x450...0x45F, 0x470...0x47F, 0x490...0x49F,
            0x510...0x51F, 0x530...0x53F, 0x550...0x55F, 0x570...0x57F, 0x590...0x59F,
            0x610, 0x612, 0x614, 0x616, 0x618, 0x61A, 0x61C, 0x61E,
            0x630, 0x632, 0x634, 0x636, 0x638, 0x63A, 0x63C, 0x63E,
            0x650, 0x652, 0x654, 0x656, 0x658, 0x65A, 0x65C, 0x65E,
            0x670, 0x672, 0x674, 0x676, 0x678, 0x67A, 0x67C, 0x67E,
            0x690, 0x692, 0x694, 0x696, 0x698, 0x69A, 0x69C, 0x69E,
            0x710, 0x712, 0x714, 0x716, 0x718, 0x71A, 0x71C, 0x71E,
            0x730, 0x732, 0x734, 0x736, 0x738, 0x73A, 0x73C, 0x73E,
            0x750, 0x752, 0x754, 0x756, 0x758, 0x75A, 0x75C, 0x75E,
            0x770, 0x772, 0x774, 0x776, 0x778, 0x77A, 0x77C, 0x77E,
            0x790, 0x792, 0x794, 0x796, 0x798, 0x79A, 0x79C, 0x79E,
            0x7B0, 0x7B2, 0x7B4, 0x7B6, 0x7B8, 0x7BA, 0x7BC, 0x7BE,
            0x7D0, 0x7D2, 0x7D4, 0x7D6, 0x7D8, 0x7DA, 0x7DC, 0x7DE,
            0x7F0, 0x7F2, 0x7F4, 0x7F6, 0x7F8, 0x7FA, 0x7FC, 0x7FE,
            => .{ .ldr = SDTransferInstr.parseSDTransfer(op, .ldr) },
            0x810...0x81F, 0x830...0x83F, 0x850...0x85F, 0x870...0x87F,
            0x890...0x89F, 0x8B0...0x8BF, 0x8D0...0x8DF, 0x8F0...0x8FF,
            0x910...0x91F, 0x930...0x93F, 0x950...0x95F, 0x970...0x97F,
            0x990...0x99F, 0x9B0...0x9BF, 0x9D0...0x9DF, 0x9F0...0x9FF,
            => .{ .ldm = BDTransferInstr.parseBDTransfer(op, .ldm) },
            0x00B, 0x00F, 0x02B, 0x02F, 0x04B, 0x04F, 0x06B, 0x06F,
            0x08B, 0x08F, 0x0AB, 0x0AF, 0x0CB, 0x0CF, 0x0EB, 0x0EF,
            0x10B, 0x10F, 0x12B, 0x12F, 0x14B, 0x14F, 0x16B, 0x16F,
            0x18B, 0x18F, 0x1AB, 0x1AF, 0x1CB, 0x1CF, 0x1EB, 0x1EF,
            0x400...0x40F, 0x420...0x42F, 0x440...0x44F, 0x460...0x46F,
            0x480...0x48F, 0x4A0...0x4AF, 0x4C0...0x4CF, 0x4E0...0x4EF,
            0x500...0x50F, 0x520...0x52F, 0x540...0x54F, 0x560...0x56F,
            0x580...0x58F, 0x5A0...0x5AF, 0x5C0...0x5CF, 0x5E0...0x5EF,
            0x600, 0x602, 0x604, 0x606, 0x608, 0x60A, 0x60C, 0x60E,
            0x620, 0x622, 0x624, 0x626, 0x628, 0x62A, 0x62C, 0x62E,
            0x640, 0x642, 0x644, 0x646, 0x648, 0x64A, 0x64C, 0x64E,
            0x660, 0x662, 0x664, 0x666, 0x668, 0x66A, 0x66C, 0x66E,
            0x680, 0x682, 0x684, 0x686, 0x688, 0x68A, 0x68C, 0x68E,
            0x6A0, 0x6A2, 0x6A4, 0x6A6, 0x6A8, 0x6AA, 0x6AC, 0x6AE,
            0x6C0, 0x6C2, 0x6C4, 0x6C6, 0x6C8, 0x6CA, 0x6CC, 0x6CE,
            0x6E0, 0x6E2, 0x6E4, 0x6E6, 0x6E8, 0x6EA, 0x6EC, 0x6EE,
            0x700, 0x702, 0x704, 0x706, 0x708, 0x70A, 0x70C, 0x70E,
            0x720, 0x722, 0x724, 0x726, 0x728, 0x72A, 0x72C, 0x72E,
            0x740, 0x742, 0x744, 0x746, 0x748, 0x74A, 0x74C, 0x74E,
            0x760, 0x762, 0x764, 0x766, 0x768, 0x76A, 0x76C, 0x76E,
            0x780, 0x782, 0x784, 0x786, 0x788, 0x78A, 0x78C, 0x78E,
            0x7A0, 0x7A2, 0x7A4, 0x7A6, 0x7A8, 0x7AA, 0x7AC, 0x7AE,
            0x7C0, 0x7C2, 0x7C4, 0x7C6, 0x7C8, 0x7CA, 0x7CC, 0x7CE,
            0x7E0, 0x7E2, 0x7E4, 0x7E6, 0x7E8, 0x7EA, 0x7EC, 0x7EE,
            => .{ .str = SDTransferInstr.parseSDTransfer(op, .str) },
            0x800...0x80F, 0x820...0x82F, 0x840...0x84F, 0x860...0x86F,
            0x880...0x88F, 0x8A0...0x8AF, 0x8C0...0x8CF, 0x8E0...0x8EF,
            0x900...0x90F, 0x920...0x92F, 0x940...0x94F, 0x960...0x96F,
            0x980...0x98F, 0x9A0...0x9AF, 0x9C0...0x9CF, 0x9E0...0x9EF,
            => .{ .stm = BDTransferInstr.parseBDTransfer(op, .stm) },
            0x109, 0x149,
            => .{ .swp = SDSwapInstr.parseSDSwap(op, .swp) },
            // zig fmt: on

            // Load/Store
            // zig fmt: off
            0xA00...0xAFF,
            => .{.b = BranchInstr.parseBranch(op, .b)},
            0xB00...0xBFF,
            => .{.bl = BranchInstr.parseBranch(op, .bl)},
            0x121,
            => .{.bx = BranchExInstr.parseBranchEx(op, .bx)},
            0x100, 0x140,
            => .{.mrs = PSRTransferInstr.parsePSRTransfer(op, .mrs)},
            0x120, 0x160,
            0x320...0x32F,
            0x360...0x36F,
            => .{.msr = PSRTransferInstr.parsePSRTransfer(op, .msr)},
            0xF00...0xFFF
            => .{.swi = undefined},
            // zig fmt: on

            else => .{ .undef = undefined },
        },
        .cond = @intToEnum(Cond, (op & 0xF0000000) >> 28),
    };
}

pub const AluOpcode = enum(u8) {
    and_ = 0x0,
    eor = 0x1,
    sub = 0x2,
    rsb = 0x3,
    add = 0x4,
    adc = 0x5,
    sbc = 0x6,
    rsc = 0x7,
    tst = 0x8,
    teq = 0x9,
    cmp = 0xA,
    cmn = 0xB,
    orr = 0xC,
    mov = 0xD,
    bic = 0xE,
    mvn = 0xF,

    pub fn isLogical(self: @This()) bool {
        return self == .mov or
            self == .mvn or
            self == .orr or
            self == .eor or
            self == .and_ or
            self == .bic or
            self == .tst or
            self == .teq;
    }
};
pub const AluInstr = struct {
    s: bool,
    op: AluOpcode,
    rd: Register,
    rn: Register,
    op2: Op2,

    fn parseAlu(op: u32, assert: AluOpcode) AluInstr {
        std.debug.assert((op & 0x01E00000) >> 21 == @enumToInt(assert));
        std.debug.assert((op & 0x0C000000) >> 26 == 0x0);
        std.debug.assert((op & 0x00000040) >> 7 == 0x0);
        const s = ((op & 0x00200000) >> 20) == 1;
        const rn = @truncate(u4, (op & 0x000F0000) >> 16);
        const rd = @truncate(u4, (op & 0x0000F000) >> 12);
        const op2 = parseOp2(op);
        if (assert == .mov or assert == .mvn)
            std.debug.assert(rn == 0x0)
        else if (assert == .cmp or assert == .cmn or assert == .tst or assert == .teq)
            std.debug.assert(rd == 0x0);
        return .{
            .s = s,
            .op = assert,
            .rd = rd,
            .rn = rn,
            .op2 = op2,
        };
    }
};

pub const MulOpcode = enum(u8) {
    mul = 0x0,
    mla = 0x1,
    umull = 0x4,
    umlal = 0x5,
    smull = 0x6,
    smlal = 0x7,
};
pub const MulInstr = struct {
    s: bool,
    op: MulOpcode,
    rd: Register, // RdHi
    rn: Register, // RdLo
    rs: Register,
    rm: Register,

    fn parseMul(op: u32, assert: MulOpcode) MulInstr {
        std.debug.assert((op & 0x01E00000) >> 21 == @enumToInt(assert));
        std.debug.assert((op & 0x0E000000) >> 25 == 0b000);
        std.debug.assert((op & 0x000000F0) >> 4 == 0b1001);
        const s = ((op & 0x00200000) >> 20) == 1;
        const rd = @truncate(u4, (op & 0x000F0000) >> 16);
        const rn = @truncate(u4, (op & 0x0000F000) >> 16);
        const rs = @truncate(u4, (op & 0x00000F00) >> 16);
        const rm = @truncate(u4, (op & 0x0000000F) >> 16);
        return .{
            .s = s,
            .op = assert,
            .rd = rd,
            .rn = rn,
            .rs = rs,
            .rm = rm,
        };
    }
};

const SDTransferOpcode = enum(u2) {
    str = 0x0,
    ldr = 0x1,
};
pub const SDTransferInstr = struct {
    when: enum(u1) { post = 0, pre = 1 },
    base_op: enum(u1) { sub = 0, add = 1 },
    size: enum(u1) { word = 0, byte = 1 },
    op: SDTransferOpcode,
    rn: Register,
    rd: Register,
    offset: union(enum) {
        imm: u12,
        reg: struct {
            shift_amt: u5,
            shift_type: enum(u2) {
                lsl = 0x0,
                lsr = 0x1,
                asr = 0x2,
                ror = 0x3,
            },
            rm: Register,
        },
    },

    fn parseSDTransfer(op: u32, assert: SDTransferOpcode) SDTransferInstr {
        std.debug.assert((op & 0x04000000) >> 26 == 0b01);
        std.debug.assert((op & 0x00100000) >> 20 == @enumToInt(assert));
        const when = @intToEnum(Field(SDTransferInstr, .when), (op & 0x01000000) >> 24);
        const base_op = @intToEnum(Field(SDTransferInstr, .base_op), (op & 0x00800000) >> 23);
        const size = @intToEnum(Field(SDTransferInstr, .size), (op & 0x00400000) >> 22);
        const rn = @truncate(u4, (op & 0x000F0000) >> 16);
        const rd = @truncate(u4, (op & 0x0000F000) >> 16);
        const offset = blk: {
            if ((op & 0x02000000) >> 20 == 0b0) {
                break :blk Field(SDTransferInstr, .offset){ .imm = (@truncate(u12, op & 0x00000FFF)) };
            } else {
                std.debug.assert((op & 0x00000008) >> 4 == 0b0);
                break :blk Field(SDTransferInstr, .offset){ .reg = .{
                    .shift_amt = @truncate(u5, (op & 0x00000F80) >> 7),
                    .shift_type = @intToEnum(Field(Field(Field(SDTransferInstr, .offset), .reg), .shift_type), (op & 0x00000F80) >> 7),
                    .rm = @truncate(u4, op & 0x0000000F),
                } };
            }
        };
        return .{
            .when = when,
            .base_op = base_op,
            .size = size,
            .op = assert,
            .rn = rn,
            .rd = rd,
            .offset = offset,
        };
    }
};

const BDTransferOpcode = enum(u2) {
    stm = 0x0,
    ldm = 0x1,
};
pub const BDTransferInstr = struct {
    when: enum(u1) { post = 0, pre = 1 },
    base_op: enum(u1) { sub = 0, add = 1 },
    force_user: enum(u1) { no = 0, force_user = 1 },
    write_back: enum(u1) { no = 0, write = 1 },
    op: BDTransferOpcode,
    rn: Register,
    reg_list: [4]Register,

    fn parseBDTransfer(op: u32, assert: BDTransferOpcode) BDTransferInstr {
        std.debug.assert((op & 0x0E000000) >> 25 == 0b100);
        std.debug.assert((op & 0x00100000) >> 20 == @enumToInt(assert));
        const when = @intToEnum(Field(BDTransferInstr, .when), (op & 0x01000000) >> 24);
        const base_op = @intToEnum(Field(BDTransferInstr, .base_op), (op & 0x00800000) >> 23);
        const force_user = @intToEnum(Field(BDTransferInstr, .force_user), (op & 0x00400000) >> 22);
        const write_back = @intToEnum(Field(BDTransferInstr, .write_back), (op & 0x00200000) >> 21);
        const rn = @truncate(u4, (op & 0x000F0000) >> 16);
        const reg_list = [4]Register{
            @truncate(u4, op & 0x0000F000 >> 12),
            @truncate(u4, op & 0x00000F00 >> 8),
            @truncate(u4, op & 0x000000F0 >> 4),
            @truncate(u4, op & 0x0000000F),
        };
        return .{
            .when = when,
            .base_op = base_op,
            .force_user = force_user,
            .write_back = write_back,
            .op = assert,
            .rn = rn,
            .reg_list = reg_list,
        };
    }
};

const SDSwapOpcode = enum(u5) { swp = 0b00010 };
pub const SDSwapInstr = struct {
    size: enum(u1) { word = 0, byte = 1 },
    op: SDSwapOpcode,
    rn: Register,
    rd: Register,
    rm: Register,

    fn parseSDSwap(op: u32, assert: SDSwapOpcode) SDSwapInstr {
        std.debug.assert((op & 0x0F800000) >> 23 == @enumToInt(assert));
        std.debug.assert((op & 0x00300000) >> 20 == 0b00);
        std.debug.assert((op & 0x00000FF0) >> 4 == 0b00001001);
        const size = @intToEnum(Field(SDSwapInstr, .size), (op & 0x00400000) >> 22);
        const rn = @truncate(u4, (op & 0x0000F000) >> 16);
        const rd = @truncate(u4, (op & 0x00000F00) >> 12);
        const rm = @truncate(u4, op & 0x0000000F);
        return .{
            .size = size,
            .op = assert,
            .rn = rn,
            .rd = rd,
            .rm = rm,
        };
    }
};

pub const BranchOpcode = enum(u1) { b = 0b0, bl = 0b1 };
pub const BranchInstr = struct {
    op: BranchOpcode,
    offset: i24,

    fn parseBranch(op: u32, assert: BranchOpcode) BranchInstr {
        std.debug.assert((op & 0x0E000000) >> 25 == 0b0101);
        std.debug.assert((op & 0x01000000) >> 24 == @enumToInt(assert));
        return .{ .op = assert, .offset = @bitCast(i24, @truncate(u24, op & 0x00FFFFFF)) };
    }
};

const BranchExOpcode = enum(u4) { bx = 0b0001 };
const BranchExInstr = struct {
    op: BranchExOpcode,
    reg: Register,

    fn parseBranchEx(op: u32, assert: BranchExOpcode) BranchExInstr {
        std.debug.assert((op & 0x0FFFFF00) >> 8 == 0x12FFF);
        std.debug.assert((op & 0x000000F0) >> 4 == @enumToInt(assert));
        return .{ .op = assert, .reg = @truncate(u4, op & 0x0000000F) };
    }
};

const PSRTransferOpcode = enum(u2) { mrs = 0b0, msr = 0b1 };
const PSRTransferInstr = struct {
    op: PSRTransferOpcode,
    source: enum(u1) {
        cpsr = 0b0,
        spsr = 0b1,
    },
    payload: union(enum) {
        mrs: struct { rd: Register },
        msr: struct {
            write_f: bool,
            write_s: bool,
            write_x: bool,
            write_c: bool,
            src: union(enum) {
                imm: struct {
                    shift_by: u4,
                    imm: u8,
                },
                reg: Register,
            },
        },
    },

    fn parsePSRTransfer(op: u32, assert: PSRTransferOpcode) PSRTransferInstr {
        std.debug.assert((op & 0x0C000000) >> 26 == 0b00);
        std.debug.assert((op & 0x01000000) >> 23 == 0b10);
        std.debug.assert((op & 0x00200000) >> 21 == @enumToInt(assert));
        std.debug.assert((op & 0x00100000) >> 20 == 0b0);
        const source = @intToEnum(Field(PSRTransferInstr, .source), (op & 0x00400000) >> 22);
        const payload = blk: {
            switch (assert) {
                .mrs => {
                    std.debug.assert((op & 0x000F0000) >> 16 == 0xF);
                    std.debug.assert(op & 0x00000FFF == 0x000);
                    const rd = @truncate(u4, (op & 0x0000F000) >> 12);
                    break :blk Field(PSRTransferInstr, .payload){ .mrs = .{
                        .rd = rd,
                    } };
                },
                .msr => {
                    std.debug.assert((op & 0x0000F000) >> 12 == 0xF);
                    const write_f = (op & 0x00080000) >> 19 == 1;
                    const write_s = (op & 0x00040000) >> 18 == 1;
                    const write_x = (op & 0x00020000) >> 17 == 1;
                    const write_c = (op & 0x00010000) >> 16 == 1;
                    const src = src: {
                        if ((op & 0x02000000) >> 25 == 0b1) {
                            break :src Field(Field(Field(PSRTransferInstr, .payload), .msr), .src){
                                .imm = .{
                                    .shift_by = @truncate(u4, (op & 0x00000F00) >> 8),
                                    .imm = @truncate(u8, op & 0x000000FF),
                                },
                            };
                        } else {
                            std.debug.assert((op & 0x00000FF0) >> 4 == 0x00);
                            break :src Field(Field(Field(PSRTransferInstr, .payload), .msr), .src){
                                .reg = @truncate(u4, op & 0x0000000F),
                            };
                        }
                    };
                    break :blk Field(PSRTransferInstr, .payload){ .msr = .{
                        .write_f = write_f,
                        .write_s = write_s,
                        .write_x = write_x,
                        .write_c = write_c,
                        .src = src,
                    } };
                },
            }
        };
        return .{ .op = assert, .source = source, .payload = payload };
    }
};

test "static analysis" {
    std.testing.refAllDecls(@This());
}
