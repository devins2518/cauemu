const std = @import("std");
const utils = @import("utils.zig");
const Field = utils.Field;

pub const Instruction = struct {
    const Self = @This();

    instr: union(enum) {
        // Logical ALU
        mov: AluInstr,
        mvn: AluInstr,
        orr: AluInstr,
        eor: AluInstr,
        @"and": AluInstr,
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

    pub fn format(self: Self, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (self.instr) {
            .b, .bl => |instr| try std.fmt.format(writer, "{}", .{instr}),
            .mov,
            .mvn,
            .orr,
            .eor,
            .@"and",
            .bic,
            .tst,
            .teq,
            => |instr| try std.fmt.format(writer, "{}", .{instr}),
            .str, .ldr => |instr| try std.fmt.format(writer, "{}", .{instr}),
            .msr, .mrs => |instr| try std.fmt.format(writer, "{}", .{instr}),
            else => try std.fmt.format(writer, "{s}", .{@tagName(self.instr)}),
        }
    }

    pub fn parse(op: u32, address: u32) Self {
        return Self{
            .cond = @intToEnum(Cond, (op & 0xF0000000) >> 28),
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
                => .{ .@"and" = AluInstr.parseAlu(op, .@"and") },
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
                => .{.b = BranchInstr.parseBranch(op, .b,address)},
                0xB00...0xBFF,
                => .{.bl = BranchInstr.parseBranch(op, .bl, address)},
                0x121,
                => .{.bx = BranchExInstr.parseBranchEx(op)},
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
        };
    }
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

    pub fn format(self: Cond, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        if (@enumToInt(self) < @enumToInt(Cond.al)) {
            try std.fmt.format(writer, "{s}", .{@tagName(self)});
        }
    }
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
    imm: u32,

    pub fn format(self: Op2, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (self) {
            .reg => |reg| {
                _ = reg;
            },
            .imm => |imm| try std.fmt.format(writer, "#0x{x}", .{imm}),
        }
    }

    fn parse(op: u32) Op2 {
        const imm = (op & 0x02000000) >> 25 == 1;
        if (imm) {
            return .{ .imm = std.math.rotr(
                u32,
                @truncate(u8, op),
                @as(u32, @truncate(u4, op >> 8)) * 2,
            ) };
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
};

pub const AluOpcode = enum(u8) {
    @"and" = 0x0,
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
            self == .@"and" or
            self == .bic or
            self == .tst or
            self == .teq;
    }
};
pub const AluInstr = struct {
    cond: Cond,
    s: bool,
    op: AluOpcode,
    rd: Register,
    rn: Register,
    op2: Op2,

    pub fn format(self: AluInstr, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try std.fmt.format(writer, "{s}{} ", .{ @tagName(self.op), self.cond });
        switch (self.op) {
            .mov, .mvn => try std.fmt.format(writer, "r{}, {}", .{ self.rd, self.op2 }),
            .cmp, .cmn, .teq, .tst => try std.fmt.format(writer, "r{}, {}", .{ self.rn, self.op2 }),
            else => try std.fmt.format(writer, "r{}, r{}, {}", .{ self.rd, self.rn, self.op2 }),
        }
    }

    fn parseAlu(op: u32, assert: AluOpcode) AluInstr {
        std.debug.assert((op & 0x01E00000) >> 21 == @enumToInt(assert));
        std.debug.assert((op & 0x0C000000) >> 26 == 0x0);
        std.debug.assert((op & 0x00000040) >> 7 == 0x0);
        const s = ((op & 0x00200000) >> 20) == 1;
        const rn = @truncate(u4, (op & 0x000F0000) >> 16);
        const rd = @truncate(u4, (op & 0x0000F000) >> 12);
        const op2 = Op2.parse(op);
        if (assert == .mov or assert == .mvn)
            std.debug.assert(rn == 0x0)
        else if (assert == .cmp or assert == .cmn or assert == .tst or assert == .teq)
            std.debug.assert(rd == 0x0);
        return .{
            .cond = @intToEnum(Cond, @truncate(u4, op >> 28)),
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
    cond: Cond,
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

    pub fn format(self: SDTransferInstr, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try std.fmt.format(writer, "{s}{}{s}{s} r{}, [r{}", .{
            @tagName(self.op),
            self.cond,
            if (self.size == .byte) "b" else "",
            if (self.when == .post) "w" else "",
            self.rd,
            self.rn,
        });
        switch (self.when) {
            .pre => {
                switch (self.offset) {
                    .imm => |imm| if (imm == 0)
                        try std.fmt.format(writer, "]", .{})
                    else
                        try std.fmt.format(writer, ", #{}]{s}", .{ imm, if (self.when == .post) "!" else "" }),
                    .reg => |reg| {
                        _ = reg;
                    },
                }
            },
            .post => {},
        }
    }

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
            .cond = @intToEnum(Cond, @truncate(u4, op >> 28)),
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
    cond: Cond,
    _: u3 = 0b101,
    op: BranchOpcode,
    offset: u32,

    fn parseBranch(op: u32, assert: BranchOpcode, address: u32) BranchInstr {
        std.debug.assert(@truncate(u3, op >> 25) == 0b101);
        std.debug.assert(@truncate(u1, op >> 24) == @enumToInt(assert));
        return BranchInstr{
            .cond = @intToEnum(Cond, @truncate(u4, op >> 28)),
            .op = assert,
            .offset = @bitCast(u32, @as(i32, @truncate(u24, op)) << 2) + address,
        };
    }

    pub fn format(self: BranchInstr, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try std.fmt.format(writer, "{s}{} #0x{x}", .{ @tagName(self.op), self.cond, self.offset + 8 });
    }
};

const BranchExInstr = packed struct(u28) {
    _: u24,
    reg: Register,

    fn parseBranchEx(op: u32) BranchExInstr {
        std.debug.assert(op & 0x0FFFFFF0 == 0x012FFF10);
        return @bitCast(BranchExInstr, @truncate(u28, op));
    }
};

const PSRTransferOpcode = enum(u2) { mrs = 0b0, msr = 0b1 };
const PSRTransferInstr = struct {
    cond: Cond,
    op: union(PSRTransferOpcode) {
        mrs: struct { rd: Register },
        msr: struct {
            write_f: bool,
            write_s: bool,
            write_x: bool,
            write_c: bool,
            src: union(enum) {
                imm: u32,
                reg: Register,
            },
        },
    },
    source: enum(u1) {
        cpsr = 0b0,
        spsr = 0b1,
    },

    pub fn format(self: PSRTransferInstr, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (self.op) {
            .mrs => |mrs| try std.fmt.format(writer, "mrs{} r{}, {s}", .{ self.cond, mrs.rd, @tagName(self.source) }),
            .msr => |msr| {
                if (msr.write_f and msr.write_s and msr.write_x and msr.write_c)
                    try std.fmt.format(writer, "msr{} {s}, r{}", .{ self.cond, @tagName(self.source), msr.src.reg })
                else
                    try std.fmt.format(writer, "msr{} {s}_{s}{s}{s}{s}, ", .{
                        self.cond,
                        @tagName(self.source),
                        if (msr.write_f) "f" else "",
                        if (msr.write_s) "s" else "",
                        if (msr.write_x) "x" else "",
                        if (msr.write_c) "c" else "",
                    });
                switch (msr.src) {
                    .imm => |imm| try std.fmt.format(writer, "#0x{x}", .{imm}),
                    .reg => |reg| try std.fmt.format(writer, "r{}", .{reg}),
                }
            },
        }
    }

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
                    break :blk Field(PSRTransferInstr, .op){ .mrs = .{
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
                            break :src Field(Field(Field(PSRTransferInstr, .op), .msr), .src){
                                .imm = std.math.rotr(u32, @truncate(u8, op), @as(u32, @truncate(u4, op >> 8))),
                            };
                        } else {
                            std.debug.assert((op & 0x00000FF0) >> 4 == 0x00);
                            break :src Field(Field(Field(PSRTransferInstr, .op), .msr), .src){
                                .reg = @truncate(u4, op & 0x0000000F),
                            };
                        }
                    };
                    break :blk Field(PSRTransferInstr, .op){ .msr = .{
                        .write_f = write_f,
                        .write_s = write_s,
                        .write_x = write_x,
                        .write_c = write_c,
                        .src = src,
                    } };
                },
            }
        };
        return .{ .cond = @intToEnum(Cond, @truncate(u4, op >> 28)), .op = payload, .source = source };
    }
};

test "instruction decoding" {
    const instrs = [_]u32{
        0xea000006, 0xea00005a, 0xea000046, 0xea000059, 0xea000058, 0xea000057, 0xea00003c, 0xea000055,
        0xe3a00301, 0xe5c00008, 0xe329f01f, 0xeb000026, 0xe3a000ff, 0xeb000319, 0xeb0009cc, 0xe3a000ff,
        0xeb000316, 0xe3a00301, 0xeb00001f, 0xe3a00301, 0xe3a01098, 0xe0d120b2, // 0xe0d130b2, 0xe18030b2,
        // 0xe35100b8, 0xbafffffa, 0xe1c000b4, 0xe1c000b8, 0xe1c004ba, 0xe2801f82, 0xe1c100b0, 0xe329f01f,
        // 0xe3a00000, 0xe3a01000, 0xe3a02000, 0xe3a03000, 0xe3a0e302, 0xe12fff1e, 0x00800000, 0x01000020,
        // 0x01000026, 0x01000030, 0x01000036, 0x880e0082, 0x02000088, 0x80000134, 0xe3a00301, 0xe5502006,
        // 0xe3120001, 0x03a0e302, 0x13a0e402, 0xe329f013, 0xe59fdf90, 0xe3a0e000, 0xe169f00e, 0xe321f012,
        // 0xe59fdf84, 0xe3a0e000, 0xe169f00e, 0xe329f01f, 0xe59fdf78, 0xe59f1f78, 0xe3a02000, 0xe7802001,
        // 0xe2911004, 0x1afffffc, 0xe8101fff, 0xe12fff1e, 0xe92d500f, 0xe3a00301, 0xe28fe000, 0xe510f004,
        // 0xe8bd500f, 0xe25ef004, 0xe92d5800, 0xe55ec002, 0xe28fb044, 0xe79bc10c, 0xe14fb000, 0xe92d0800,
        // 0xe20bb080, 0xe38bb01f, 0xe129f00b, 0xe92d4004, 0xe28fe000, 0xe12fff1c, 0xe8bd4004, 0xe3a0c093,
        // 0xe129f00c, 0xe8bd0800, 0xe169f00b, 0xe8bd5800, 0xe1b0f00e, 0xeafffffe, 0xeafffffe, 0x000000b8,
        // 0x00000ca0, 0x00000bfc, 0x00000c0c, 0x00000c30, 0x00000c28, 0x00000734, 0x00000728, 0x00000798,
        // 0x00000840, 0x000007ec, 0x00000234, 0x000002c4, 0x0000022c, 0x00000b58, 0x00000ae0, 0x00000450,
        // 0x00000328, 0x000003ac, 0x0000058c, 0x00000624, 0x00000690, 0x000004d0, 0x00000508, 0x00000554,
        // 0x00000228, 0x00000fac, 0x00000228, 0x00000228, 0x00001140, 0x00000228, 0x00000ea4, 0x00000228,
        // 0x00000228, 0x00000228, 0x00000228, 0x00000228, 0x00000228, 0x00000020, 0x00000c1c, 0x000010d4,
        // 0x000010c0, 0x00000f08, 0xe12fff1e, 0xe59f0e44, 0xe12fff1e, 0xe92d0018, 0xe1a03582, 0xe1b035a3,
        // 0x0a00001d, 0xe1b02ca2, 0x23822001, 0xe28f4000, 0xe794f102, 0x00000264, 0x0000027c, 0x00000294,
        // 0x000002ac, 0xe0d040b2, 0xe0c140b2, 0xe2533001, 0x1afffffb, 0xe8bd0018, 0xe12fff1e, 0xe1d040b0,
        // 0xe0c140b2, 0xe2533001, 0x1afffffc, 0xe8bd0018, 0xe12fff1e, 0xe8b00010, 0xe8a10010, 0xe2533001,
        // 0x1afffffb, 0xe8bd0018, 0xe12fff1e, 0xe5904000, 0xe4814004, 0xe2533001, 0x1afffffc, 0xe8bd0018,
        // 0xe12fff1e, 0xe92d0ff8, 0xe1a03582, 0xe1b035a3, 0x0a000012, 0xe3120401, 0x1a000005, 0xe8b00ff0,
        // 0xe8a10ff0, 0xe2533008, 0xcafffffb, 0xe8bd0ff8, 0xe12fff1e, 0xe5904000, 0xe1a05004, 0xe1a06004,
        // 0xe1a07004, 0xe1a08004, 0xe1a09004, 0xe1a0a004, 0xe1a0b004, 0xe8a10ff0, 0xe2533008, 0xcafffffc,
        // 0xe8bd0ff8, 0xe12fff1e, 0xe92d0078, 0xe4902004, 0xe1b02422, 0x0a00001a, 0xe4d03001, 0xe3833401,
        // 0xe3130080, 0x1a000006, 0xe4d04001, 0xe4c14001, 0xe2522001, 0x0a000012, 0xe1b03083, 0x3afffff7,
        // 0xeafffff4, 0xe4d04001, 0xe4d05001, 0xe1855404, 0xe3c55a0f, 0xe2855001, 0xe1a04224, 0xe2844003,
        // 0xe7516005, 0xe4c16001, 0xe2522001, 0x0a000004, 0xe2544001, 0x1afffff9, 0xe1b03083, 0x3affffe7,
        // 0xeaffffe4, 0xe8bd0078, 0xe12fff1e, 0xe92d00f8, 0xe4902004, 0xe1b02422, 0x0a000022, 0xe4d03001,
        // 0xe3833401, 0xe3130080, 0x1a00000a, 0xe4d04001, 0xe3110001, 0x01a07004, 0x11877404, 0x11c170b0,
        // 0xe2811001, 0xe2522001, 0x0a000016, 0xe1b03083, 0x3afffff3, 0xeafffff0, 0xe4d04001, 0xe4d05001,
        // 0xe1855404, 0xe3c55a0f, 0xe2855001, 0xe1a04224, 0xe2844003, 0xe7516005, 0xe3110001, 0x01a07006,
        // 0x11877406, 0x11c170b0, 0xe2811001, 0xe2522001, 0x0a000004, 0xe2544001, 0x1afffff5, 0xe1b03083,
        // 0x3affffdf, 0xeaffffdc, 0xe8bd00f8, 0xe12fff1e, 0xe92d1ffc, 0xe1d230b0, 0xe3530000, 0x11b04ca0,
        // 0x0a000018, 0xe5d24002, 0xe5d25003, 0xe5922004, 0xe1a06fa2, 0xe3c22102, 0xe3a0c001, 0xe06cc41c,
        // 0xe3a07000, 0xe3a09000, 0xe2533001, 0x4a00000d, 0xe4d0b001, 0xe3a08000, 0xe3580008, 0xaafffff9,
        // 0xe01ca83b, 0x03560000, 0x108aa002, 0xe187791a, 0xe0899005, 0xe219901f, 0x04817004, 0x03a07000,
        // 0xe0888004, 0xeafffff3, 0xe8bd1ffc, 0xe12fff1e, 0xe92d001c, 0xe4902004, 0xe1a02422, 0xe3520000,
        // 0x11b03ca0, 0x0a000005, 0xe4d03001, 0xe4c13001, 0xe4d04001, 0xe0833004, 0xe2522001, 0xcafffffa,
        // 0xe8bd001c, 0xe12fff1e, 0xe92d003c, 0xe4902004, 0xe1a02422, 0xe3520000, 0x11b03ca0, 0x0a00000a,
        // 0xe4d03001, 0xe4d04001, 0xe1834404, 0xe0844403, 0xe0c140b2, 0xe4d03001, 0xe0833424, 0xe20330ff,
        // 0xe4d04001, 0xe2522002, 0xcafffff6, 0xe8bd003c, 0xe12fff1e, 0xe92d001c, 0xe4902004, 0xe1a02422,
        // 0xe3520000, 0x11b03ca0, 0x0a000005, 0xe0d030b2, 0xe0c130b2, 0xe0d040b2, 0xe0833004, 0xe2522002,
        // 0xcafffffa, 0xe8bd001c, 0xe12fff1e, 0xe92d0ffc, 0xe4903004, 0xe1b02423, 0x11b02ca0, 0x0a00001e,
        // 0xe203200f, 0xe1a03423, 0xe4d04001, 0xe0804084, 0xe2844001, 0xe3a05000, 0xe3a06000, 0xe1a07000,
        // 0xe3a08020, 0xe4949004, 0xe2588001, 0xbafffffb, 0xe5d7a000, 0xe20ab03f, 0xe28bb001, 0xe3c77001,
        // 0xe1b09089, 0xe0a7708b, 0x21a0a08a, 0xe31a0080, 0x0afffff4, 0xe5d77000, 0xe1855617, 0xe1a07000,
        // 0xe0866002, 0xe216601f, 0x1affffee, 0xe4815004, 0xe3a05000, 0xe2533004, 0xcaffffea, 0xe8bd0ffc,
        // 0xe12fff1e, 0xe92d001c, 0xe4902004, 0xe1a02422, 0xe3520000, 0x11b03ca0, 0x0a000012, 0xe4d03001,
        // 0xe1b03c83, 0xe1a03ca3, 0x3a000006, 0xe4d04001, 0xe2833003, 0xe0422003, 0xe4c14001, 0xe2533001,
        // 0xcafffffc, 0xea000005, 0xe2833001, 0xe0422003, 0xe4d04001, 0xe4c14001, 0xe2533001, 0xcafffffb,
        // 0xe3520000, 0xcaffffec, 0xe8bd001c, 0xe12fff1e, 0xe92d007c, 0xe4902004, 0xe1a02422, 0xe3520000,
        // 0x11b03ca0, 0x0a00001d, 0xe3a05000, 0xe3a06000, 0xe4d03001, 0xe1b03c83, 0xe1a03ca3, 0x3a00000a,
        // 0xe4d04001, 0xe2833003, 0xe0422003, 0xe2533001, 0xba000010, 0xe1866514, 0xe2355008, 0x1afffffa,
        // 0xe0c160b2, 0xe3a06000, 0xeafffff7, 0xe2833001, 0xe0422003, 0xe2533001, 0xba000006, 0xe4d04001,
        // 0xe1866514, 0xe2355008, 0x1afffff9, 0xe0c160b2, 0xe3a06000, 0xeafffff6, 0xe3520000, 0xcaffffe3,
        // 0xe8bd007c, 0xe12fff1e, 0xe1a03001, 0xe1a01000, 0xe1a00003, 0xe92d0014, 0xe1b04001, 0x42711000,
        // 0x0a000013, 0xe0344040, 0x22600000, 0xe3a02001, 0xe15100a0, 0x91a02082, 0x91a01081, 0x9afffffb,
        // 0xe3a03000, 0xe1510000, 0xd0400001, 0xd0833002, 0x11a010a1, 0x11b020a2, 0x1afffff9, 0xe0301044,
        // 0xe2a11000, 0xe0330044, 0xe2a00000, 0xe8bd0014, 0xe12fff1e, 0xeafffffc, 0xe92d001e, 0xe3a01001,
        // 0xe1a02000, 0xe1510122, 0x91a01081, 0x91a020a2, 0x9afffffb, 0xe1a020a1, 0xe0813002, 0xe0040393,
        // 0xe1540000, 0x90811002, 0x11b020a2, 0x1afffff9, 0xe1a00001, 0xe8bd001e, 0xe12fff1e, 0x00400000,
        // 0x00800040, 0x00c00080, 0x010000c0, 0xe92d4034, 0xe1a02000, 0xe1a03001, 0xe3a04000, 0xe3510000,
        // 0xb2844008, 0xb2633000, 0xe0315040, 0x42844004, 0x22622000, 0xe0522003, 0x51a03001, 0x51a01000,
        // 0x51a00003, 0xe0355002, 0x42844002, 0xe59f3848, 0xe19340b4, 0xe1a00700, 0xebffffbd, 0xe59fe83c,
        // 0xe0010090, 0xe1a01741, 0xe2611000, 0xe3a030a9, 0xe0030391, 0xe1a03743, 0xe2833e39, 0xe0030391,
        // 0xe1a03743, 0xe2833c09, 0xe283301c, 0xe0030391, 0xe1a03743, 0xe2833c0f, 0xe28330b6, 0xe0030391,
        // 0xe1a03743, 0xe2833c16, 0xe28330aa, 0xe0030391, 0xe1a03743, 0xe2833a02, 0xe2833081, 0xe0030391,
        // 0xe1a03743, 0xe2833c36, 0xe2833051, 0xe0030391, 0xe1a03743, 0xe2833ca2, 0xe28330f9, 0xe0000093,
        // 0xe1a00840, 0xe12fff1e, 0xe0300042, 0xe2a00000, 0xe0800404, 0xe3a03e17, 0xe8bd4034, 0xe12fff1e,
        // 0x01920000, 0x04b50323, 0x07d50645, 0x0af10964, 0x0e050c7c, 0x11110f8c, 0x14131294, 0x1708158f,
        // 0x19ef187d, 0x1cc61b5d, 0x1f8b1e2b, 0x223d20e7, 0x24da238e, 0x275f261f, 0x29cd2899, 0x2c212afa,
        // 0x2e5a2d41, 0x30762f6b, 0x32743179, 0x34533367, 0x36123536, 0x37af36e5, 0x392a3871, 0x3a8239da,
        // 0x3bb63b20, 0x3cc53c42, 0x3dae3d3e, 0x3e713e14, 0x3f0e3ec5, 0x3f843f4e, 0x3fd33fb1, 0x3ffb3fec,
        // 0x3ffb4000, 0x3fd33fec, 0x3f843fb1, 0x3f0e3f4e, 0x3e713ec5, 0x3dae3e14, 0x3cc53d3e, 0x3bb63c42,
        // 0x3a823b20, 0x392a39da, 0x37af3871, 0x361236e5, 0x34533536, 0x32743367, 0x30763179, 0x2e5a2f6b,
        // 0x2c212d41, 0x29cd2afa, 0x275f2899, 0x24da261f, 0x223d238e, 0x1f8b20e7, 0x1cc61e2b, 0x19ef1b5d,
        // 0x1708187d, 0x1413158f, 0x11111294, 0x0e050f8c, 0x0af10c7c, 0x07d50964, 0x04b50645, 0x01920323,
        // 0xfe6e0000, 0xfb4bfcdd, 0xf82bf9bb, 0xf50ff69c, 0xf1fbf384, 0xeeeff074, 0xebeded6c, 0xe8f8ea71,
        // 0xe611e783, 0xe33ae4a3, 0xe075e1d5, 0xddc3df19, 0xdb26dc72, 0xd8a1d9e1, 0xd633d767, 0xd3dfd506,
        // 0xd1a6d2bf, 0xcf8ad095, 0xcd8cce87, 0xcbadcc99, 0xc9eecaca, 0xc851c91b, 0xc6d6c78f, 0xc57ec626,
        // 0xc44ac4e0, 0xc33bc3be, 0xc252c2c2, 0xc18fc1ec, 0xc0f2c13b, 0xc07cc0b2, 0xc02dc04f, 0xc005c014,
        // 0xc005c000, 0xc02dc014, 0xc07cc04f, 0xc0f2c0b2, 0xc18fc13b, 0xc252c1ec, 0xc33bc2c2, 0xc44ac3be,
        // 0xc57ec4e0, 0xc6d6c626, 0xc851c78f, 0xc9eec91b, 0xcbadcaca, 0xcd8ccc99, 0xcf8ace87, 0xd1a6d095,
        // 0xd3dfd2bf, 0xd633d506, 0xd8a1d767, 0xdb26d9e1, 0xddc3dc72, 0xe075df19, 0xe33ae1d5, 0xe611e4a3,
        // 0xe8f8e783, 0xebedea71, 0xeeefed6c, 0xf1fbf074, 0xf50ff384, 0xf82bf69c, 0xfb4bf9bb, 0xfe6efcdd,
        // 0xe92d1bf0, 0xe59fc598, 0xe3a0b000, 0xe2522001, 0x4a000016, 0xe0d040f2, 0xe0d060f2, 0xe0d070b2,
        // 0xe1a07427, 0xe1a08087, 0xe2879040, 0xe20990ff, 0xe1a09089, 0xe19c80f8, 0xe19c90f9, 0xe0050894,
        // 0xe04b5745, 0xe0040499, 0xe1a04744, 0xe0070996, 0xe1a07747, 0xe0060698, 0xe1a06746, 0xe08140b3,
        // 0xe08150b3, 0xe08160b3, 0xe08170b3, 0xeaffffe6, 0xe8bd1bf0, 0xe12fff1e, 0xe92d1ff8, 0xe59fc520,
        // 0xe2522001, 0x4a000022, 0xe8b00e00, 0xe0d030f2, 0xe0d050f2, 0xe0d060b4, 0xe1a06426, 0xe1a07086,
        // 0xe2868040, 0xe20880ff, 0xe1a08088, 0xe19c70f7, 0xe19c80f8, 0xe0040793, 0xe1a04744, 0xe0030398,
        // 0xe1a03743, 0xe0060895, 0xe1a06746, 0xe0050597, 0xe1a05745, 0xe1a0780b, 0xe1a07847, 0xe2677000,
        // 0xe1a0884b, 0xe0299397, 0xe0299498, 0xe02aa597, 0xe2688000, 0xe02aa698, 0xe0c130b2, 0xe2644000,
        // 0xe0c140b2, 0xe0c150b2, 0xe0c160b2, 0xe8a10600, 0xeaffffda, 0xe8bd1ff8, 0xe12fff1e, 0xe3a0b000,
        // 0xe3a0c301, 0xe5ccb301, 0xe12fff1e, 0xe3a0b080, 0xe3a0c301, 0xe5ccb301, 0xe12fff1e, 0xe3a0c301,
        // 0xe5cc2301, 0xe12fff1e, 0xe3a00001, 0xe3a01001, 0xe92d4010, 0xe3a02000, 0xe3a03001, 0xe3a0c301,
        // 0xe3500000, 0x1b000001, 0xe28fe01c, 0xe5cc2301, 0xe5cc2208, 0xe15c40b8, 0xe0110004, 0x10244000,
        // 0x114c40b8, 0xe5cc3208, 0xe12fff1e, 0x0afffff6, 0xe8bd4010, 0xe12fff1e, 0x00000000, 0x02000000,
        // 0x03000000, 0x05000000, 0x06000000, 0x07000000, 0x1f80ffff, 0x60000100, 0x00000100, 0xe1a00000,
        // 0xe92d581e, 0xe1a03000, 0xe59f03d8, 0xe3130080, 0x1a00002c, 0xe3130040, 0x1a000019, 0xe3130020,
        // 0x1a00000c, 0xe203301f, 0xe3a04000, 0xe59fb3b8, 0xe59fc3b8, 0xe59fe3b8, 0xe2844002, 0xe1b030a3,
        // 0x0a000032, 0x3afffffb, 0xe79b1084, 0xe19c20b4, 0xe3822401, 0xeafffd72, 0xe59f1398, 0xe3a02020,
        // 0xe3822401, 0xebfffd6e, 0xe3a01301, 0xe2811c01, 0xe3a02902, 0xe1c123b4, 0xe3a02007, 0xe1c124b0,
        // 0xeaffffe7, 0xe59f1370, 0xe3a02008, 0xe3822401, 0xebfffd63, 0xe3a01301, 0xe3a02080, 0xe1c118b0,
        // 0xe1c118b2, 0xe1c118b4, 0xe1c128b4, 0xe1d128b8, 0xe3c22b3f, 0xe1c128b8, 0xe3a02070, 0xe1c127b0,
        // 0xe1c118b4, 0xeaffffd4, 0xe3a01301, 0xe3a02401, 0xe2822018, 0xebfffd52, 0xe2811050, 0xebfffd50,
        // 0xe28110f0, 0xe3a04000, 0xe4814004, 0xe4814004, 0xe5814000, 0xe2411c02, 0xe3a04c01, 0xe1c142b0,
        // 0xe1c142b6, 0xe1c143b0, 0xe1c143b6, 0xeaffffc0, 0xe8bd581e, 0xe12fff1e, 0xe1a00000, 0xe1a00000,
        // 0xe3e2e1e0, 0xe7e6e5e4, 0xebeae9e8, 0xd3d2d1d0, 0xd7d6d5d4, 0xdbdad9d8, 0xc3c2c1c0, 0xc7c6c5c4,
        // 0xcbcac9c8, 0xb3b2b1b0, 0xb7b6b5b4, 0xbbbab9b8, 0xa3a2a1a0, 0xa7a6a5a4, 0xabaaa9a8, 0x93929190,
        // 0x97969594, 0x9b9a9998, 0x83828180, 0x87868584, 0x8b8a8988, 0x73727170, 0x77767574, 0x7b7a7978,
        // 0x63626160, 0x67666564, 0x6b6a6968, 0x53525150, 0x57565554, 0x5b5a5958, 0x43424140, 0x47464544,
        // 0x4b4a4948, 0x33323130, 0x37363534, 0x3b3a3938, 0x23222120, 0x27262524, 0x2b2a2928, 0x13121110,
        // 0x17161514, 0x1b1a1918, 0x03020100, 0x07060504, 0x0b0a0908, 0x80000000, 0x879c7c97, 0x8facd61e,
        // 0x9837f052, 0xa14517cc, 0xaadc0848, 0xb504f334, 0xbfc886bb, 0xcb2ff52a, 0xd744fccb, 0xe411f03a,
        // 0xf1a1bf39, 0xe92d0070, 0xe1a02c02, 0xe35100b2, 0xca000011, 0xe59f31e4, 0xe28340b4, 0xe7f35001,
        // 0xe205600f, 0xe7946106, 0xe1a05225, 0xe1a06536, 0xe5d33001, 0xe203100f, 0xe7944101, 0xe1a03223,
        // 0xe0664334, 0xe0813294, 0xe0811006, 0xe5902004, 0xe0803192, 0xe8bd0070, 0xe12fff1e, 0xe3a024ff,
        // 0xe3a010b2, 0xeaffffea, 0xe59f2194, 0xe3a03023, 0xe7802003, 0xe2533001, 0xaafffffc, 0xe12fff1e,
        // 0x00000228, 0x00000228, 0x00000228, 0x00000228, 0x00000228, 0x00000228, 0x00000228, 0x00000228,
        // 0x00000228, 0x00000228, 0x00000228, 0x00000228, 0x00000228, 0x00000228, 0x00000228, 0x00000228,
        // 0x00000228, 0x00000228, 0x00000228, 0x00000228, 0x00000228, 0x00000228, 0x00000228, 0x00000228,
        // 0x00000228, 0x00000228, 0x00000228, 0x00000228, 0x00000228, 0x00000228, 0x00000228, 0x00000228,
        // 0x00000228, 0x00000228, 0x00000228, 0xe92d4010, 0xe1a04000, 0xe3a01301, 0xe1c11cb6, 0xe1c11db2,
        // 0xe3a0208f, 0xe1c128b4, 0xe1df2db8, 0xe1c128b2, 0xe5d12089, 0xe3822040, 0xe5c12089, 0xe2802e35,
        // 0xe58120bc, 0xe28120a0, 0xe58120c0, 0xe2802d26, 0xe58120c8, 0xe28120a4, 0xe58120cc, 0xe59f20a8,
        // 0xe5820000, 0xe3a00000, 0xe92d0001, 0xe24d0004, 0xe1a01004, 0xe3a02ffb, 0xe3822401, 0xebfffc84,
        // 0xe8bd0001, 0xe3a00008, 0xe5c40006, 0xe3a0000f, 0xe5c40007, 0xe1df07b4, 0xe5840038, 0xe59f0070,
        // 0xe5840028, 0xe584002c, 0xe5840030, 0xe584003c, 0xe59f0060, 0xe5840034, 0xe59f005c, 0xe5840000,
        // 0xe8bd4010, 0xe12fff1e, 0x03007fe0, 0x03007fa0, 0x03007f00, 0xfffffe00, 0xbaae187f, 0x000007dc,
        // 0x000008c8, 0x000008e0, 0x00000c78, 0x00000c78, 0x00000c8e, 0x00000cd8, 0x04000120, 0x04000060,
        // 0x00000dc0, 0x00000000, 0x0000a90e, 0x03007ff0, 0x00002425, 0x00000228, 0x00000f20, 0x68736d53,
        // 0xe3a01cb6, 0xe3a00301, 0xe1c01cb6, 0xe1c01db2, 0xe12fff1e, 0xe92d4010, 0xe59f40b0, 0xe5944000,
        // 0xe5941000, 0xe59f00a8, 0xe0400001, 0xe3500001, 0x8a000010, 0xe2811001, 0xe5841000, 0xe3a01301,
        // 0xe1c11cb6, 0xe1c11db2, 0xe5c41004, 0xe3a00000, 0xe92d0001, 0xe24d0004, 0xe2841e35, 0xe3a02fc6,
        // 0xe3822401, 0xebfffc42, 0xe8bd0001, 0xe5941000, 0xe2411001, 0xe5841000, 0xe8bd4010, 0xe12fff1e,
        // 0xe59f0048, 0xe5900000, 0xe5901000, 0xe59f2044, 0xe1510002, 0x1a00000c, 0xe5d01004, 0xe2511001,
        // 0xe5c01004, 0xca000008, 0xe5d0100b, 0xe5c01004, 0xe3a00000, 0xe3a01cb6, 0xe3a02301, 0xe1c20cb6,
        // 0xe1c20db2, 0xe1c21cb6, 0xe1c21db2, 0xe12fff1e, 0x03007ff0, 0x68736d54, 0x68736d53, 0xe1a00000,
        // 0x00300030, 0x009e00ff, 0x80f8f801, 0x820080fc, 0x83008d01, 0x7c7c03fc, 0x00930101, 0x3e3c7c02,
        // 0x3e007f81, 0xff0100b8, 0x807f80ff, 0xffff0100, 0x008d7f80, 0x00823f80, 0x00d33f80, 0x81c0c001,
        // 0x00ff01e0, 0xe104c382, 0x070700ff, 0x0f000381, 0xff050085, 0xf8f0f0ff, 0x81ff81f8, 0xffff09f0,
        // 0x01010f0f, 0x0f0f0000, 0xff020085, 0x3c807c7c, 0xff3e3e04, 0x7c807878, 0x0f3c3c02, 0xf0000093,
        // 0x3f000084, 0xf807008d, 0x7e7efcfc, 0x80fcfefe, 0x87e704ff, 0x8d0f0707, 0xf8fc0200, 0x04c080e0,
        // 0x3f1fcfc3, 0x8dff827f, 0x00ff8000, 0x03e081fe, 0x1f3f7fff, 0x00b10180, 0xfe80f800, 0x00801f00,
        // 0x0f070304, 0x0080cf8f, 0x0f1f3e04, 0x0085070f, 0xf800fe80, 0xef0a0081, 0x78fbf7ef, 0x03fffe7c,
        // 0x00800103, 0x867f3f01, 0xc0801600, 0xf8f0e0e0, 0xf7ffef7c, 0xe0e1f1f3, 0xf9ff7fc0, 0x7f7ffff9,
        // 0x0400b03f, 0xfefefcf0, 0x0400807f, 0x7e3f3f1f, 0x0700877e, 0xfefcf080, 0xbf3f7ffe, 0xfe09ff81,
    };
    var instr_strs = std.ArrayList(u8).init(std.testing.allocator);
    for (instrs) |instr, i| {
        const instruction = Instruction.parse(instr, @intCast(u32, i) * 4);
        try std.fmt.format(instr_strs.writer(), "{}\n", .{instruction});
    }

    defer instr_strs.deinit();
    try std.testing.expectEqualStrings(
        \\b #0x20
        \\b #0x174
        \\b #0x128
        \\b #0x178
        \\b #0x178
        \\b #0x178
        \\b #0x110
        \\b #0x178
        \\mov r0, #0x4000000
        \\strb r0, [r0, #8]
        \\msr cpsr_fc, #0x1f
        \\bl #0xcc
        \\mov r0, #0xff
        \\bl #0xca0
        \\bl #0x2770
        \\mov r0, #0xff
        \\bl #0xca0
        \\mov r0, #0x4000000
        \\bl #0xcc
        \\mov r0, #0x4000000
        \\mov r1, #0x98
        \\ldrh r2, [r1], #2
        \\ldrh r3, [r1], #2
        \\strh r3, [r0, r2]
        \\cmp r1, #0xb8
        \\blt #0x54
        \\strh r0, [r0, #4]
        \\strh r0, [r0, #8]
        \\strh r0, [r0, #0x4a]
        \\add r1, r0, #0x208
        \\strh r0, [r1]
        \\msr cpsr_fc, #0x1f
        \\mov r0, #0
        \\mov r1, #0
        \\mov r2, #0
        \\mov r3, #0
        \\mov lr, #0x8000000
        \\bx lr
        \\addeq r0, r0, r0
        \\lsls     r3, r3, #0x19
        \\lsr      r3, r3, #0x19
        \\blo      #0x28
        \\ldrb     r4, [r0], #1
        \\add      r3, r3, #3
        \\sub      r2, r2, r3
        \\strb     r4, [r1], #1
        \\subs     r3, r3, #1
        \\bgt      #0x18
        \\b        #0x40
        \\add      r3, r3, #1
        \\sub      r2, r2, r3
        \\ldrb     r4, [r0], #1
        \\strb     r4, [r1], #1
        \\subs     r3, r3, #1
        \\bgt      #0x30
        \\cmp      r2, #0
        \\bgt      #0xfffffffc
        \\pop      {r2, r3, r4}
        \\bx       lr
        \\push     {r2, r3, r4, r5, r6}
        \\ldr      r2, [r0], #4
        \\lsr      r2, r2, #8
        \\cmp      r2, #0
        \\lsrsne   r3, r0, #0x19
        \\beq      #0xe0
        \\mov      r5, #0
        \\mov      r6, #0
        \\ldrb     r3, [r0], #1
        \\lsls     r3, r3, #0x19
        \\lsr      r3, r3, #0x19
        \\blo      #0xac
        \\ldrb     r4, [r0], #1
        \\add      r3, r3, #3
        \\sub      r2, r2, r3
        \\subs     r3, r3, #1
        \\blt      #0xd8
        \\orr      r6, r6, r4, lsl r5
        \\eors     r5, r5, #8
        \\bne      #0x8c
        \\strh     r6, [r1], #2
        \\mov      r6, #0
        \\b        #0x8c
        \\add      r3, r3, #1
        \\sub      r2, r2, r3
        \\subs     r3, r3, #1
        \\blt      #0xd8
        \\ldrb     r4, [r0], #1
        \\orr      r6, r6, r4, lsl r5
        \\eors     r5, r5, #8
        \\bne      #0xb4
        \\strh     r6, [r1], #2
        \\mov      r6, #0
        \\b        #0xb4
        \\cmp      r2, #0
        \\bgt      #0x70
        \\pop      {r2, r3, r4, r5, r6}
        \\bx       lr
        \\mov      r3, r1
        \\mov      r1, r0
        \\mov      r0, r3
        \\push     {r2, r4}
        \\movs     r4, r1
        \\rsbsmi   r1, r1, #0
        \\beq      #0x154
        \\eors     r4, r4, r0, asr #32
        \\rsbhs    r0, r0, #0
        \\mov      r2, #1
        \\cmp      r1, r0, lsr #1
        \\lslls    r2, r2, #1
        \\lslls    r1, r1, #1
        \\bls      #0x110
        \\mov      r3, #0
        \\cmp      r1, r0
        \\suble    r0, r0, r1
        \\addle    r3, r3, r2
        \\lsrne    r1, r1, #1
        \\lsrsne   r2, r2, #1
        \\bne      #0x124
        \\eors     r1, r0, r4, asr #32
        \\adc      r1, r1, #0
        \\eors     r0, r3, r4, asr #32
        \\adc      r0, r0, #0
        \\pop      {r2, r4}
        \\bx       lr
        \\b        #0x14c
        \\push     {r1, r2, r3, r4}
        \\mov      r1, #1
        \\mov      r2, r0
        \\cmp      r1, r2, lsr #2
        \\lslls    r1, r1, #1
        \\lsrls    r2, r2, #1
        \\bls      #0x164
        \\lsr      r2, r1, #1
        \\add      r3, r1, r2
        \\mul      r4, r3, r3
        \\cmp      r4, r0
        \\addls    r1, r1, r2
        \\lsrsne   r2, r2, #1
        \\bne      #0x178
        \\mov      r0, r1
        \\pop      {r1, r2, r3, r4}
        \\bx       lr
        \\subeq    r0, r0, r0
        \\addeq    r0, r0, r0, asr #32
        \\sbceq    r0, r0, r0, lsl #1
        \\smlabteq r0, r0, r0, r0
        \\push     {r2, r4, r5, lr}
        \\mov      r2, r0
        \\mov      r3, r1
        \\mov      r4, #0
        \\cmp      r1, #0
        \\addlt    r4, r4, #8
        \\rsblt    r3, r3, #0
        \\eors     r5, r1, r0, asr #32
        \\addmi    r4, r4, #4
        \\rsbhs    r2, r2, #0
        \\subs     r2, r2, r3
        \\movpl    r3, r1
        \\movpl    r1, r0
        \\movpl    r0, r3
        \\eors     r5, r5, r2
        \\addmi    r4, r4, #2
        \\ldr      r3, [pc, #0x848]
        \\ldrh     r4, [r3, r4]
        \\lsl      r0, r0, #0xe
        \\bl       #0xf4
        \\ldr      lr, [pc, #0x83c]
        \\mul      r1, r0, r0
        \\asr      r1, r1, #0xe
        \\rsb      r1, r1, #0
        \\mov      r3, #0xa9
        \\mul      r3, r1, r3
        \\asr      r3, r3, #0xe
        \\add      r3, r3, #0x390
        \\mul      r3, r1, r3
        \\asr      r3, r3, #0xe
        \\add      r3, r3, #0x900
        \\add      r3, r3, #0x1c
        \\mul      r3, r1, r3
        \\asr      r3, r3, #0xe
        \\add      r3, r3, #0xf00
        \\add      r3, r3, #0xb6
        \\mul      r3, r1, r3
        \\asr      r3, r3, #0xe
        \\add      r3, r3, #0x1600
        \\add      r3, r3, #0xaa
        \\mul      r3, r1, r3
        \\asr      r3, r3, #0xe
        \\add      r3, r3, #0x2000
        \\add      r3, r3, #0x81
        \\mul      r3, r1, r3
        \\asr      r3, r3, #0xe
        \\add      r3, r3, #0x3600
        \\add      r3, r3, #0x51
        \\mul      r3, r1, r3
        \\asr      r3, r3, #0xe
        \\add      r3, r3, #0xa200
        \\add      r3, r3, #0xf9
        \\mul      r0, r3, r0
        \\asr      r0, r0, #0x10
        \\bx       lr
        \\eors     r0, r0, r2, asr #32
        \\adc      r0, r0, #0
        \\add      r0, r0, r4, lsl #8
        \\mov      r3, #0x170
        \\pop      {r2, r4, r5, lr}
        \\bx       lr
        \\orrseq   r0, r2, r0
        \\ldrteq   r0, [r5], #0x323
        \\ldrbeq   r0, [r5, r5, asr #12]
        \\beq      #0xffc42844
        \\mcreq    p12, #0, r0, c5, c12, #3
        \\tstne    r1, ip, lsl #31
        \\ldrne    r1, [r3], #-0x294
        \\strne    r1, [r8, -pc, lsl #11]
        \\stmibne  pc!, {r0, r2, r3, r4, r5, r6, fp, ip} ^
        \\movweq r0, #0
        \\streq  r0, [r0, #-0]
        \\streq  r0, [r0], -r0
        \\streq  r0, [r0, -r0]
        \\svcne  #0x80ffff
        \\andvs  r0, r0, r0, lsl #2
        \\andeq  r0, r0, r0, lsl #2
        \\mov    r0, r0
        \\push   {r1, r2, r3, r4, fp, ip, lr}
        \\mov    r3, r0
        \\ldr    r0, [pc, #0x3d8]
        \\tst    r3, #0x80
        \\bne    #0xe8
        \\tst    r3, #0x40
        \\bne    #0xa4
        \\tst    r3, #0x20
        \\bne    #0x78
        \\and    r3, r3, #0x1f
        \\mov    r4, #0
        \\ldr    fp, [pc, #0x3b8]
        \\ldr    ip, [pc, #0x3b8]
        \\ldr    lr, [pc, #0x3b8]
        \\add    r4, r4, #2
        \\lsrs   r3, r3, #1
        \\beq    #0x130
        \\blo    #0x58
        \\ldr    r1, [fp, r4, lsl #1]
        \\ldrh   r2, [ip, r4]
        \\orr    r2, r2, #0x1000000
        \\b      #0xfffff644
        \\ldr    r1, [pc, #0x398]
        \\mov    r2, #0x20
        \\orr    r2, r2, #0x1000000
        \\bl     #0xfffff644
        \\mov    r1, #0x4000000
        \\add    r1, r1, #0x100
        \\mov    r2, #0x8000
        \\strh   r2, [r1, #0x34]
        \\mov    r2, #7
        \\strh   r2, [r1, #0x40]
        \\b      #0x44
        \\ldr    r1, [pc, #0x370]
        \\mov    r2, #8
        \\orr    r2, r2, #0x1000000
        \\bl     #0xfffff644
        \\mov    r1, #0x4000000
        \\mov    r2, #0x80
        \\strh   r1, [r1, #0x80]
        \\strh   r1, [r1, #0x82]
        \\strh   r1, [r1, #0x84]
        \\strh   r2, [r1, #0x84]
        \\ldrh   r2, [r1, #0x88]
        \\bic    r2, r2, #0xfc00
        \\strh   r2, [r1, #0x88]
        \\mov    r2, #0x70
        \\strh   r2, [r1, #0x70]
        \\strh   r1, [r1, #0x84]
        \\b      #0x3c
        \\mov    r1, #0x4000000
        \\mov    r2, #0x1000000
        \\add    r2, r2, #0x18
        \\bl     #0xfffff644
        \\add    r1, r1, #0x50
        \\bl     #0xfffff644
        \\add    r1, r1, #0xf0
        \\mov    r4, #0
        \\str    r4, [r1], #4
        \\str    r4, [r1], #4
        \\str    r4, [r1]
        \\sub    r1, r1, #0x200
        \\mov    r4, #0x100
        \\strh   r4, [r1, #0x20]
        \\strh   r4, [r1, #0x26]
        \\strh   r4, [r1, #0x30]
        \\strh   r4, [r1, #0x36]
        \\b      #0x34
        \\pop    {r1, r2, r3, r4, fp, ip, lr}
        \\bx     lr
        \\mov    r0, r0
        \\mov    r0, r0
    , instr_strs.items);
}
