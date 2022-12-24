const std = @import("std");
const utils = @import("utils.zig");
const Field = utils.Field;
const matches = @import("matches").matches;

pub const Instruction = union(enum) {
    const Self = @This();
    alu: AluInstr,
    mul: MulInstr,
    sd_transfer: SDTransferInstr,
    hsd_transfer: HSDTransferInstr,
    bd_transfer: BDTransferInstr,
    sd_swp: SDSwapInstr,
    branch: BranchInstr,
    branchx: BranchExInstr,
    psr_transfer: PSRTransferInstr,
    swi,
    undef,

    pub fn format(self: Self, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (self) {
            .alu => |instr| try std.fmt.format(writer, "{}", .{instr}),
            .mul => |instr| try std.fmt.format(writer, "{}", .{instr}),
            .sd_transfer => |instr| try std.fmt.format(writer, "{}", .{instr}),
            .hsd_transfer => |instr| try std.fmt.format(writer, "{}", .{instr}),
            .bd_transfer => |instr| try std.fmt.format(writer, "{}", .{instr}),
            .sd_swp => |instr| try std.fmt.format(writer, "{}", .{instr}),
            .branch => |instr| try std.fmt.format(writer, "{}", .{instr}),
            .branchx => |instr| try std.fmt.format(writer, "{}", .{instr}),
            .psr_transfer => |instr| try std.fmt.format(writer, "{}", .{instr}),
            .swi => |instr| try std.fmt.format(writer, "{}", .{instr}),
            .undef => try std.fmt.format(writer, "undef", .{}),
        }
    }

    pub fn parse(op: u32, address: u32) Self {
        const bits4 = @truncate(u4, op >> 4);
        const bits20 = @truncate(u8, op >> 20);
        // TODO: Redo this with u32 values
        return if ((matches(bits20, "0b0000000x") and (matches(bits4, "0b0xxx") or matches(bits4, "0b1xx0"))) or matches(bits20, "0b0010000x"))
            Instruction{ .alu = AluInstr.parseAlu(op, .@"and") }
        else if ((matches(bits20, "0b0000001x") and (matches(bits4, "0b0xxx") or matches(bits4, "0b1xx0"))) or matches(bits20, "0b0010001x"))
            Instruction{ .alu = AluInstr.parseAlu(op, .eor) }
        else if ((matches(bits20, "0b0000010x") and (matches(bits4, "0b0xxx") or matches(bits4, "0b1xx0"))) or matches(bits20, "0b0010010x"))
            Instruction{ .alu = AluInstr.parseAlu(op, .sub) }
        else if ((matches(bits20, "0b0000011x") and (matches(bits4, "0b0xxx") or matches(bits4, "0b1xx0"))) or matches(bits20, "0b0010011x"))
            Instruction{ .alu = AluInstr.parseAlu(op, .rsb) }
        else if ((matches(bits20, "0b0000100x") and (matches(bits4, "0b0xxx") or matches(bits4, "0b1xx0"))) or matches(bits20, "0b0010100x"))
            Instruction{ .alu = AluInstr.parseAlu(op, .add) }
        else if ((matches(bits20, "0b0000101x") and (matches(bits4, "0b0xxx") or matches(bits4, "0b1xx0"))) or matches(bits20, "0b0010101x"))
            Instruction{ .alu = AluInstr.parseAlu(op, .adc) }
        else if ((matches(bits20, "0b0000110x") and (matches(bits4, "0b0xxx") or matches(bits4, "0b1xx0"))) or matches(bits20, "0b0010110x"))
            Instruction{ .alu = AluInstr.parseAlu(op, .sbc) }
        else if ((matches(bits20, "0b0000111x") and (matches(bits4, "0b0xxx") or matches(bits4, "0b1xx0"))) or matches(bits20, "0b0010111x"))
            Instruction{ .alu = AluInstr.parseAlu(op, .rsc) }
            // TODO: Check this
        else if ((matches(bits20, "0b0001000x") and (matches(bits4, "0b0xxx") or matches(bits4, "0b1xx0"))) or matches(bits20, "0b0011000x"))
            Instruction{ .alu = AluInstr.parseAlu(op, .tst) }
        else if ((matches(bits20, "0b00010011") and (matches(bits4, "0b0xxx") or matches(bits4, "0b1xx0"))) or matches(bits20, "0b00110011"))
            Instruction{ .alu = AluInstr.parseAlu(op, .teq) }
        else if ((matches(bits20, "0b00010101") and (matches(bits4, "0b0xxx") or matches(bits4, "0b1xx0"))) or matches(bits20, "0b0011010x"))
            Instruction{ .alu = AluInstr.parseAlu(op, .cmp) }
        else if ((matches(bits20, "0b00010111") and (matches(bits4, "0b0xxx") or matches(bits4, "0b1xx0"))) or matches(bits20, "0b00110111"))
            Instruction{ .alu = AluInstr.parseAlu(op, .cmn) }
        else if ((matches(bits20, "0b0001100x") and (matches(bits4, "0b0xxx") or matches(bits4, "0b1xx0"))) or matches(bits20, "0b0011100x"))
            Instruction{ .alu = AluInstr.parseAlu(op, .orr) }
        else if ((matches(bits20, "0b0001100x") and (matches(bits4, "0b0xxx") or matches(bits4, "0b1xx0"))) or matches(bits20, "0b0011100x"))
            Instruction{ .alu = AluInstr.parseAlu(op, .orr) }
        else if ((matches(bits20, "0b0001101x") and (matches(bits4, "0b0xxx") or matches(bits4, "0b1xx0"))) or matches(bits20, "0b0011101x"))
            Instruction{ .alu = AluInstr.parseAlu(op, .mov) }
        else if ((matches(bits20, "0b0001110x") and (matches(bits4, "0b0xxx") or matches(bits4, "0b1xx0"))) or matches(bits20, "0b0011110x"))
            Instruction{ .alu = AluInstr.parseAlu(op, .bic) }
        else if ((matches(bits20, "0b0001111x") and (matches(bits4, "0b0xxx") or matches(bits4, "0b1xx0"))) or matches(bits20, "0b0011111x"))
            Instruction{ .alu = AluInstr.parseAlu(op, .mvn) }
        else if (matches(bits20, "0b0000000x") and matches(bits4, "0b1001"))
            Instruction{ .mul = MulInstr.parseMul(op, .mul) }
        else if (matches(bits20, "0b0000001x") and matches(bits4, "0b1001"))
            Instruction{ .mul = MulInstr.parseMul(op, .mla) }
        else if (matches(bits20, "0b0000100x") and matches(bits4, "0b1001"))
            Instruction{ .mul = MulInstr.parseMul(op, .umull) }
        else if (matches(bits20, "0b0000101x") and matches(bits4, "0b1001"))
            Instruction{ .mul = MulInstr.parseMul(op, .umlal) }
        else if (matches(bits20, "0b0000110x") and matches(bits4, "0b1001"))
            Instruction{ .mul = MulInstr.parseMul(op, .smull) }
        else if ((matches(bits20, "0b000x111x") or matches(bits20, "0b0001101x") or matches(bits20, "0b00011x0x")) and matches(bits4, "0b1001"))
            Instruction{ .mul = MulInstr.parseMul(op, .smlal) }
        else if (matches(bits20, "0b00010x00") and matches(bits4, "0b1001"))
            Instruction{ .sd_swp = SDSwapInstr.parseSDSwap(op, .swp) }
        else if (matches(bits20, "0b000xxxx0") and matches(bits4, "0b1011"))
            Instruction{ .hsd_transfer = HSDTransferInstr.parseHSDTransfer(op, .strh) }
        else if (matches(bits20, "0b000xxxx1") and matches(bits4, "0b1011"))
            Instruction{ .hsd_transfer = HSDTransferInstr.parseHSDTransfer(op, .ldrh) }
        else if (matches(bits20, "0b000xxxx1") and matches(bits4, "0b1101"))
            Instruction{ .hsd_transfer = HSDTransferInstr.parseHSDTransfer(op, .ldrsb) }
        else if (matches(bits20, "0b000xxxx1") and matches(bits4, "0b1111"))
            Instruction{ .hsd_transfer = HSDTransferInstr.parseHSDTransfer(op, .ldrsh) }
        else if (matches(bits20, "0b00010x00") and matches(bits4, "0b0000"))
            Instruction{ .psr_transfer = PSRTransferInstr.parsePSRTransfer(op, .mrs) }
        else if ((matches(bits20, "0b00010x10") and matches(bits4, "0b0000")) or matches(bits20, "0b00110x10"))
            Instruction{ .psr_transfer = PSRTransferInstr.parsePSRTransfer(op, .msr) }
        else if (matches(op, "0bxxxx000100101111111111110001xxxx"))
            Instruction{ .branchx = BranchExInstr.parseBranchEx(op) }
        else if (matches(op, "0bxxxx01xxxxx0xxxxxxxxxxxxxxxxxxxx"))
            Instruction{ .sd_transfer = SDTransferInstr.parseSDTransfer(op, .str, address) }
        else if (matches(op, "0bxxxx01xxxxx1xxxxxxxxxxxxxxxxxxxx"))
            Instruction{ .sd_transfer = SDTransferInstr.parseSDTransfer(op, .ldr, address) }
        else if (matches(op, "0bxxxx100xxxx0xxxxxxxxxxxxxxxxxxxx"))
            Instruction{ .bd_transfer = BDTransferInstr.parseBDTransfer(op, .stm) }
        else if (matches(op, "0bxxxx100xxxx1xxxxxxxxxxxxxxxxxxxx"))
            Instruction{ .bd_transfer = BDTransferInstr.parseBDTransfer(op, .ldm) }
            // Coprocessor instructions not implemented
        else if (matches(bits20, "0b1010xxxx"))
            Instruction{ .branch = BranchInstr.parseBranch(op, .b, address) }
        else if (matches(bits20, "0b1011xxxx"))
            Instruction{ .branch = BranchInstr.parseBranch(op, .bl, address) }
        else if (matches(bits20, "0b1111xxxx"))
            Instruction{ .swi = undefined }
        else
            Instruction{ .undef = undefined };
    }
};

pub const Register = struct {
    r: u4,

    pub fn format(self: Register, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try switch (self.r) {
            10 => std.fmt.format(writer, "sl", .{}),
            11 => std.fmt.format(writer, "fp", .{}),
            12 => std.fmt.format(writer, "ip", .{}),
            13 => std.fmt.format(writer, "sp", .{}),
            14 => std.fmt.format(writer, "lr", .{}),
            15 => std.fmt.format(writer, "pc", .{}),
            else => std.fmt.format(writer, "r{}", .{self.r}),
        };
    }

    pub fn from(n: anytype) Register {
        return Register{ .r = @truncate(u4, n) };
    }
};

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
            imm: u6,
            reg: Register,
        },
        shift_type: enum(u2) {
            lsl = 0x0,
            lsr = 0x1,
            asr = 0x2,
            ror = 0x3,
        },
        reg: Register,
    },
    imm: u32,

    pub fn format(self: Op2, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (self) {
            .reg => |reg| switch (reg.shift_by) {
                .imm => |imm| if (imm == 0)
                    try std.fmt.format(writer, "{}", .{reg.reg})
                else
                    try std.fmt.format(writer, "{}, {s} 0x{x}", .{ reg.reg, @tagName(reg.shift_type), imm }),
                .reg => |rm| try std.fmt.format(writer, "{}, {s} {}", .{ reg.reg, @tagName(reg.shift_type), rm }),
            },
            .imm => |imm| try std.fmt.format(writer, "0x{x}", .{imm}),
        }
    }

    fn parse(op: u32) Op2 {
        const imm = @truncate(u1, op >> 25) == 1;
        if (imm) {
            return Op2{
                .imm = std.math.rotr(u32, @truncate(u8, op), @as(u32, @truncate(u4, op >> 8)) * 2),
            };
        } else {
            const RegTy = Field(Op2, .reg);
            const ShiftByTy = Field(RegTy, .shift_by);
            const ShiftTypeTy = Field(RegTy, .shift_type);
            var reg = RegTy{
                .shift_by = if (@truncate(u1, op >> 4) == 0) blk: {
                    break :blk ShiftByTy{ .imm = @truncate(u5, op >> 7) };
                } else blk: {
                    break :blk ShiftByTy{ .reg = Register.from(op >> 8) };
                },
                .shift_type = @intToEnum(ShiftTypeTy, @truncate(u2, op >> 5)),
                .reg = Register.from(op),
            };

            if (reg.shift_by == .imm and reg.shift_by.imm == 0 and reg.shift_type != .lsl)
                reg.shift_by.imm = 32;

            return Op2{ .reg = reg };
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
        const s = if (self.s and (@enumToInt(self.op) < @enumToInt(AluOpcode.tst) or @enumToInt(self.op) > @enumToInt(AluOpcode.cmn)))
            "s"
        else
            "";
        try std.fmt.format(writer, "{s}{}{s} ", .{ @tagName(self.op), self.cond, s });
        switch (self.op) {
            .mov, .mvn => try std.fmt.format(writer, "{}, {}", .{ self.rd, self.op2 }),
            .cmp, .cmn, .teq, .tst => try std.fmt.format(writer, "{}, {}", .{ self.rn, self.op2 }),
            else => try std.fmt.format(writer, "{}, {}, {}", .{ self.rd, self.rn, self.op2 }),
        }
    }

    fn parseAlu(op: u32, assert: AluOpcode) AluInstr {
        std.debug.assert((op & 0x01E00000) >> 21 == @enumToInt(assert));
        std.debug.assert((op & 0x0C000000) >> 26 == 0x0);
        std.debug.assert((op & 0x00000040) >> 7 == 0x0);
        const s = @truncate(u1, op >> 20) == 1;
        const rn = Register.from(op >> 16);
        const rd = Register.from(op >> 12);
        const op2 = Op2.parse(op);
        if (assert == .mov or assert == .mvn)
            std.debug.assert(rn.r == 0x0)
        else if (assert == .cmp or assert == .cmn or assert == .tst or assert == .teq)
            std.debug.assert(rd.r == 0x0);
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
        const rd = Register.from(op >> 16);
        const rn = Register.from(op >> 12);
        const rs = Register.from(op >> 8);
        const rm = Register.from(op);
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
    indexing: enum(u1) { post = 0, pre = 1 },
    u: enum(u1) { down = 0, up = 1 },
    size: enum(u1) { word = 0, byte = 1 },
    w: bool,
    op: SDTransferOpcode,
    rn: Register,
    rd: Register,
    offset: union(enum) {
        imm: u32,
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
        try std.fmt.format(writer, "{s}{}{s}{s} {}, [{}", .{
            @tagName(self.op),
            self.cond,
            if (self.size == .byte) "b" else "",
            if (self.w) "w" else "",
            self.rd,
            self.rn,
        });
        const minus = if (self.u == .down) "-" else "";
        const exclaim = if (self.indexing == .post) "!" else "";
        switch (self.indexing) {
            .pre => switch (self.offset) {
                .imm => |imm| if (imm == 0)
                    try std.fmt.format(writer, "]", .{})
                else if (self.rn.r == 15)
                    try std.fmt.format(writer, ", {s}0x{x}]{s}", .{ minus, imm + 8, exclaim })
                else
                    try std.fmt.format(writer, ", {s}0x{x}]{s}", .{ minus, imm, exclaim }),
                .reg => |reg| if (reg.shift_amt == 0)
                    try std.fmt.format(writer, ", {s}{}]{s}", .{ minus, reg.rm, exclaim })
                else
                    try std.fmt.format(writer, ", {s}{}, {s} 0x{x}]{s}", .{ minus, reg.rm, @tagName(reg.shift_type), reg.shift_amt, exclaim }),
            },
            .post => {},
        }
    }

    fn parseSDTransfer(op: u32, assert: SDTransferOpcode, address: u32) SDTransferInstr {
        std.debug.assert(@truncate(u2, op >> 26) == 0b01);
        std.debug.assert(@truncate(u1, op >> 20) == @enumToInt(assert));
        const OffsetTy = Field(SDTransferInstr, .offset);
        const RegTy = Field(OffsetTy, .reg);
        const ShiftTypeTy = Field(RegTy, .shift_type);
        const indexing = @intToEnum(Field(SDTransferInstr, .indexing), @truncate(u1, op >> 24));
        const u = @intToEnum(Field(SDTransferInstr, .u), @truncate(u1, op >> 23));
        const size = @intToEnum(Field(SDTransferInstr, .size), @truncate(u1, op >> 22));
        const w = @truncate(u1, op >> 21) == 1;
        const rn = Register.from(op >> 16);
        const rd = Register.from(op >> 12);
        var offset = blk: {
            if ((op & 0x02000000) >> 20 == 0b0) {
                break :blk OffsetTy{ .imm = (@truncate(u12, op)) };
            } else {
                std.debug.assert((op & 0x00000008) >> 4 == 0b0);
                break :blk OffsetTy{ .reg = .{
                    .shift_amt = @truncate(u5, op >> 7),
                    .shift_type = @intToEnum(ShiftTypeTy, @truncate(u2, op >> 5)),
                    .rm = Register.from(op),
                } };
            }
        };
        if (rn.r == 15 and offset == .imm) offset.imm += address;
        return .{
            .cond = @intToEnum(Cond, @truncate(u4, op >> 28)),
            .indexing = indexing,
            .u = u,
            .size = size,
            .w = w,
            .op = assert,
            .rn = rn,
            .rd = rd,
            .offset = offset,
        };
    }
};

const HSDTransferOpcode = enum(u3) {
    // 0bx00 filtered out by SDSwapInstr
    ldrh = 0b101,
    strh = 0b001,
    ldrsb = 0b110,
    ldrsh = 0b111,
};
// TODO: writeback bit
pub const HSDTransferInstr = struct {
    cond: Cond,
    indexing: enum(u1) { post = 0, pre = 1 },
    u: enum(u1) { down = 0, up = 1 },
    w: bool,
    op: HSDTransferOpcode,
    rd: Register,
    rn: Register,
    offset: union(enum) {
        imm: u8,
        rm: Register,
    },

    pub fn format(self: HSDTransferInstr, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try std.fmt.format(writer, "{s}{} {}, [{}", .{
            @tagName(self.op),
            self.cond,
            self.rd,
            self.rn,
        });
        const minus = if (self.u == .down) "-" else "";
        switch (self.indexing) {
            .pre => switch (self.offset) {
                .imm => |imm| if (imm == 0)
                    try std.fmt.format(writer, "]", .{})
                else
                    try std.fmt.format(writer, ", 0x{x}]", .{imm}),
                .rm => |rm| try std.fmt.format(writer, ", {s}{}]", .{ minus, rm }),
            },
            .post => {
                try std.fmt.format(writer, "], ", .{});
                switch (self.offset) {
                    .imm => |imm| try std.fmt.format(writer, "0x{x}", .{imm}),
                    .rm => |rm| try std.fmt.format(writer, "{s}{}", .{ minus, rm }),
                }
            },
        }
    }

    fn parseHSDTransfer(op: u32, assert: HSDTransferOpcode) HSDTransferInstr {
        std.debug.assert(@intToEnum(HSDTransferOpcode, @as(u3, @truncate(u1, op >> 20)) << 2 |
            @truncate(u2, op >> 5)) == assert);
        return HSDTransferInstr{
            .cond = @intToEnum(Cond, @truncate(u4, op >> 28)),
            .indexing = @intToEnum(Field(HSDTransferInstr, .indexing), @truncate(u1, op >> 24)),
            .u = @intToEnum(Field(HSDTransferInstr, .u), @truncate(u1, op >> 23)),
            .w = @truncate(u1, op >> 21) == 1,
            .op = assert,
            .rd = Register.from(op >> 12),
            .rn = Register.from(op >> 16),
            .offset = if (@truncate(u1, op >> 22) == 0)
                Field(HSDTransferInstr, .offset){ .rm = Register.from(op) }
            else
                Field(HSDTransferInstr, .offset){ .imm = @as(u8, @truncate(u4, op >> 8)) << 4 | @truncate(u4, op) },
        };
    }
};

const BDTransferOpcode = enum(u2) {
    stm = 0x0,
    ldm = 0x1,
};
pub const BDTransferInstr = struct {
    cond: Cond,
    indexing: enum(u1) { post = 0, pre = 1 },
    u: enum(u1) { down = 0, up = 1 },
    load_psr_force_user: bool,
    w: bool,
    op: BDTransferOpcode,
    rn: Register,
    reg_list: u16,

    pub fn format(self: BDTransferInstr, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        if (self.op == .stm and self.rn.r == 13 and self.w and @popCount(self.reg_list) >= 2)
            try std.fmt.format(writer, "push{s} {{", .{self.cond})
        else if (self.op == .ldm and self.rn.r == 13 and self.w and @popCount(self.reg_list) >= 2)
            try std.fmt.format(writer, "pop{s} {{", .{self.cond})
        else {
            // TODO: refactor
            const addr_mode = if (self.rn.r == 13 and self.indexing == .pre and self.u == .up)
                if (self.op == .ldm)
                    "ed"
                else
                    "fa"
            else if (self.rn.r == 13 and self.indexing == .post and self.u == .up)
                if (self.op == .ldm)
                    "fd"
                else
                    "ea"
            else if (self.rn.r == 13 and self.indexing == .pre and self.u == .down)
                if (self.op == .ldm)
                    "ea"
                else
                    "fd"
            else if (self.rn.r == 13)
                if (self.op == .ldm)
                    "fa"
                else
                    "ed"
            else if (self.indexing == .pre and self.u == .up)
                "ib"
            else if (self.indexing == .post and self.u == .up)
                "ia"
            else if (self.indexing == .pre and self.u == .down)
                "db"
            else
                "da";
            try std.fmt.format(writer, "{s}{s}{} {}{s}, {{", .{ @tagName(self.op), addr_mode, self.cond, self.rn, if (self.w) "!" else "" });
        }
        comptime var i = 0;
        inline while (i <= 15) : (i += 1) {
            if (@truncate(u1, self.reg_list >> i) == 1) {
                try std.fmt.format(writer, "{}", .{Register.from(i)});
                if (self.reg_list >> i > 1)
                    try std.fmt.format(writer, ", ", .{});
            }
        }
        try std.fmt.format(writer, "}}", .{});
    }

    fn parseBDTransfer(op: u32, assert: BDTransferOpcode) BDTransferInstr {
        std.debug.assert((op & 0x0E000000) >> 25 == 0b100);
        std.debug.assert((op & 0x00100000) >> 20 == @enumToInt(assert));
        const indexing = @intToEnum(Field(BDTransferInstr, .indexing), @truncate(u1, op >> 24));
        const u = @intToEnum(Field(BDTransferInstr, .u), @truncate(u1, op >> 23));
        const load_psr_force_user = @truncate(u1, op >> 22) == 1;
        const w = @truncate(u1, op >> 21) == 1;
        const rn = Register.from(op >> 16);
        const reg_list = @truncate(u16, op);
        return .{
            .cond = @intToEnum(Cond, @truncate(u4, op >> 28)),
            .indexing = indexing,
            .u = u,
            .load_psr_force_user = load_psr_force_user,
            .w = w,
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
        const rn = Register.from(op >> 16);
        const rd = Register.from(op >> 12);
        const rm = Register.from(op);
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
        var offset = @as(u32, @bitCast(u24, @truncate(i24, @bitCast(i32, op))));
        if (offset >> 23 == 0b1) offset |= 0xff000000;
        return BranchInstr{
            .cond = @intToEnum(Cond, @truncate(u4, op >> 28)),
            .op = assert,
            .offset = (offset << 2) +% address,
        };
    }

    pub fn format(self: BranchInstr, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try std.fmt.format(writer, "{s}{} 0x{x}", .{ @tagName(self.op), self.cond, self.offset + 8 });
    }
};

const BranchExInstr = struct {
    cond: Cond,
    reg: Register,

    fn parseBranchEx(op: u32) BranchExInstr {
        std.debug.assert(matches(op, "0bxxxx000100101111111111110001xxxx"));
        return BranchExInstr{
            .cond = @intToEnum(Cond, @truncate(u4, op >> 28)),
            .reg = Register.from(op),
        };
    }

    pub fn format(self: BranchExInstr, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try std.fmt.format(writer, "bx {}", .{self.reg});
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
            .mrs => |mrs| try std.fmt.format(writer, "mrs{} {}, {s}", .{ self.cond, mrs.rd, @tagName(self.source) }),
            .msr => |msr| {
                if (msr.write_f and msr.write_s and msr.write_x and msr.write_c)
                    try std.fmt.format(writer, "msr{} {s}, {}", .{ self.cond, @tagName(self.source), msr.src.reg })
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
                    .imm => |imm| try std.fmt.format(writer, "0x{x}", .{imm}),
                    .reg => |reg| try std.fmt.format(writer, "{}", .{reg}),
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
                    const rd = Register.from(op >> 12);
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
                                .reg = Register.from(op),
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
        0xeb000316, 0xe3a00301, 0xeb00001f, 0xe3a00301, 0xe3a01098, 0xe0d120b2, 0xe0d130b2, 0xe18030b2,
        0xe35100b8, 0xbafffffa, 0xe1c000b4, 0xe1c000b8, 0xe1c004ba, 0xe2801f82, 0xe1c100b0, 0xe329f01f,
        0xe3a00000, 0xe3a01000, 0xe3a02000, 0xe3a03000, 0xe3a0e302, 0xe12fff1e, 0x00800000, 0x01000020,
        0x01000026, 0x01000030, 0x01000036, 0x880e0082, 0x02000088, 0x80000134, 0xe3a00301, 0xe5502006,
        0xe3120001, 0x03a0e302, 0x13a0e402, 0xe329f013, 0xe59fdf90, 0xe3a0e000, 0xe169f00e, 0xe321f012,
        0xe59fdf84, 0xe3a0e000, 0xe169f00e, 0xe329f01f, 0xe59fdf78, 0xe59f1f78, 0xe3a02000, 0xe7802001,
        0xe2911004, 0x1afffffc, 0xe8101fff, 0xe12fff1e, 0xe92d500f, 0xe3a00301, 0xe28fe000, 0xe510f004,
        0xe8bd500f, 0xe25ef004, 0xe92d5800, 0xe55ec002, 0xe28fb044, 0xe79bc10c, 0xe14fb000, 0xe92d0800,
        0xe20bb080, 0xe38bb01f, 0xe129f00b, 0xe92d4004, 0xe28fe000, 0xe12fff1c, 0xe8bd4004, 0xe3a0c093,
        0xe129f00c, 0xe8bd0800, 0xe169f00b, 0xe8bd5800, 0xe1b0f00e, 0xeafffffe, 0xeafffffe, 0x000000b8,
        0x00000ca0, 0x00000bfc, 0x00000c0c, 0x00000c30, 0x00000c28, 0x00000734, 0x00000728, 0x00000798,
        0x00000840, 0x000007ec, 0x00000234, 0x000002c4, 0x0000022c, 0x00000b58, 0x00000ae0, // 0x00000450,
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
        // 0x3f3f7efe, 0x7e00071f, 0x0700857f, 0xff7f3f3f, 0xf0fcfefe, 0xff81fc80, 0x3f3fbf05, 0x80ffff1f,
        // 0x0400a87f, 0xfffffefe, 0x0200957f, 0xbe3f3f7f, 0xe0c00300, 0x0081f8f0, 0x03070703, 0x04008d01,
        // 0x7efcfcf8, 0x003f807e, 0x01009401, 0x1f833f3f, 0x1f070095, 0x3e3e3f3f, 0x997c7c7e, 0xf0f00300,
        // 0x0081e0e0, 0x03010103, 0x00008d03, 0x80c084e0, 0x8d0f8207, 0x80c08100, 0x82f000e0, 0x8d07800f,
        // 0xf0f00a00, 0x7efcf8f8, 0x03031f3e, 0x8000b501, 0xcfc601e0, 0x07040080, 0xfb630307, 0xff80008d,
        // 0xf8f0c00c, 0xfffd7e7e, 0x1f0703ff, 0x008d3e3f, 0x83387c01, 0x1e3e0100, 0x808000b8, 0x0f800082,
        // 0x8080008d, 0x0f80ff82, 0x0080ff82, 0x00850382, 0x00018083, 0xd40f8300, 0xf8f80200, 0x020082fc,
        // 0x8d030301, 0xfcfc0900, 0xfcf0f0f8, 0x07077fff, 0x01000380, 0xfe0100d5, 0x010083fe, 0x008d1f1f,
        // 0xfffefe02, 0x0f800082, 0x7e0500f4, 0xfefffffe, 0x0400c07e, 0xf0e0e0c0, 0x040080f8, 0x0307070f,
        // 0x0b008a01, 0xfcc0c080, 0x3f3f7efc, 0x010f0f1f, 0xe00b008c, 0xfcf8f8f0, 0x073f7e7e, 0xb4010103,
        // 0xf0c00400, 0x80fcfcf8, 0x7f3f0100, 0x0083ff80, 0x85010101, 0xfefe0200, 0x017f817e, 0xf883fd3f,
        // 0x8201fc01, 0x01010103, 0x7f800085, 0xfefeff0e, 0xfcfcf8fc, 0x3f7ffffe, 0x01010f1f, 0xf88100ae,
        // 0x0080fc00, 0x01810300, 0xfc82008d, 0x0180fe80, 0xfe000092, 0x7f807e80, 0x00b83f00, 0xf0f0e004,
        // 0x0080f8f8, 0x807f3f01, 0x000084ff, 0x01008501, 0x00821838, 0xfc808000, 0x7ffefe04, 0x01803f7f,
        // 0xc00a008a, 0xf8f0f0e0, 0x1ffffefc, 0xff820f1f, 0x01000080, 0xf00400ac, 0xfcfcf8f8, 0x3f010080,
        // 0x84ff807f, 0x85010000, 0x001c0100, 0xfc08f083, 0x3ffffffc, 0x017f7f3f, 0x0f00008e, 0xfe02ff81,
        // 0x7f807e7f, 0x0f1f3f02, 0x800000b4, 0xfc040080, 0xfffffefc, 0x01810080, 0xc0060086, 0xf8f0f0e0,
        // 0xff837efc, 0xff7e7e02, 0x03000084, 0xff810085, 0xff810081, 0x03803f81, 0x00ac0100, 0x0080f082,
        // 0x0080ff82, 0x00890100, 0xdc09f883, 0x0f010100, 0x7f7f3f3f, 0x01008fff, 0xff80fe0e, 0xfefefc08,
        // 0x3f7f7fff, 0x00b0071f, 0xe0c08004, 0x0080f0e0, 0x1f3f3f04, 0x008d0f0f, 0xfcfcf80b, 0xfffffefe,
        // 0x3f03077f, 0x00ff807f, 0x010083fc, 0x00850101, 0xff0e3f80, 0xf8fcfeff, 0xfffefcfc, 0x0f3f7fff,
        // 0x00ae0101, 0xff01fe80, 0x800080ff, 0x7f7f01ff, 0x01000080, 0x8003008a, 0x80c0c080, 0x3f7f04e0,
        // 0x801f1f3f, 0x01008d0f, 0xf880f0f0, 0xfefcfc06, 0x03030707, 0x00b10180, 0xf0e0c004, 0x0080f8f8,
        // 0x0081ff82, 0x03800100, 0xf8810085, 0xfefcf004, 0xff80f1fe, 0xffff7f06, 0x010303fc, 0x01010080,
        // 0x80008501, 0xfeff077f, 0xf8f0fcfe, 0xff80fcf8, 0x821f7f01, 0x0400ab01, 0xfefefcf0, 0x040080ff,
        // 0xff7f7f1f, 0x03008dff, 0x3f3f7f7f, 0xfe00ff80, 0xff80fc81, 0x01817f00, 0xfc0f0089, 0xf0e0e0c0,
        // 0x7ffcf8f8, 0x0f0f1f3f, 0xcd010307, 0x81f80000, 0x80f800fc, 0x92018100, 0xfe7e0500, 0x7efeffff,
        // 0xe00000b8, 0x0080f081, 0x0f010780, 0x09008d07, 0x0000c0e0, 0xfcfcf8f0, 0x00800307, 0x03030102,
        // 0xfc09008d, 0xf0f0f8fc, 0x071f7ffe, 0x00038007, 0x0200bc01, 0x81fef8c0, 0x85038100, 0xf0c00200,
        // 0x07ff84fe, 0x00010f3f, 0x01031f03, 0xfc02008b, 0x008280e0, 0xffff7f05, 0x8080f0fc, 0x00038200,
        // 0x0000ad02, 0x000084ff, 0x000084ff, 0x81008503, 0x000001ff, 0x0003ff83, 0x81ffff00, 0x00000303,
        // 0x00850303, 0x0082ff80, 0x0082ff80, 0x00ae0380, 0x7f1f0303, 0x000084ff, 0x03008d03, 0xc0f0feff,
        // 0xe00b0080, 0xffff3f0f, 0xfffffcfe, 0x82010000, 0x00008503, 0x07ff80f8, 0x00010f3f, 0x01071fff,
        // 0xfc0000b4, 0xff00fe80, 0x07040080, 0x1f1f0f0f, 0xc70d008d, 0xf8f0e0c3, 0x1f7cfcfc, 0x070f0f1f,
        // 0x02008f03, 0x813e7e7e, 0x9a3e007f, 0xc0800200, 0x030081f0, 0x3ffffffe, 0x3f030081, 0x83fcffff,
        // 0x07031300, 0x3e7cf8f8, 0xcfcf9e9e, 0xfffefc03, 0x070f1fbf, 0x7f81ffe0, 0x0f3e3e04, 0x1e801f0f,
        // 0x843c1c01, 0x81df00cf, 0x87873207, 0x3f3effff, 0xdf9f1f1f, 0x1e1cffff, 0x0f0f1f1e, 0x3ebe0307,
        // 0xe0f0f8fc, 0x7fff0080, 0xff3f0300, 0x7ffffeff, 0xfffcc000, 0x3c017fff, 0x070f1f3f, 0x0400a903,
        // 0xfefefcfc, 0x820080ff, 0x06008603, 0xc0c08080, 0x80f0f0e0, 0xefef04ff, 0x82c3c7e7, 0x850f8007,
        // 0xf8f80700, 0xfefefcfc, 0xff807f7f, 0x8003c080, 0x830f0f80, 0x8200a81f, 0x800080f8, 0xe1fd01ff,
        // 0x01040080, 0x0f0f0707, 0xfc840085, 0xe1e1fe03, 0x09ff80fd, 0x0f0fe0fc, 0x07030307, 0x00850f0f,
        // 0x7e01fe80, 0x80ff807e, 0xfce001c0, 0x1f80ff80, 0x07010f80, 0x0300a901, 0xf8f8f0c0, 0xff820080,
        // 0x1f810080, 0x00850f00, 0xfe80fc00, 0x0f027f81, 0x00820103, 0x008c0c00, 0xfe80ff00, 0xf0f8fc06,
        // 0x830000c0, 0x0801ff82, 0x020f800e, 0xa8010707, 0x80f88200, 0x80ff8200, 0x07010400, 0x853f1f0f,
        // 0x05fc8300, 0x81e1fefe, 0x00810101, 0x7f803f00, 0x7f7e7e03, 0x0400857f, 0xfe7efefe, 0x02ff80fe,
        // 0x82e08000, 0x7f7f07ff, 0x0f1f3f3f, 0x00a80107, 0x0080f882, 0x0080ff82, 0x03810700, 0xfc840085,
        // 0x0101fe02, 0x008eff82, 0x7efefe04, 0xff80fefe, 0xff820080, 0x03810080, 0x00a80100, 0x0080f882,
        // 0x0080ff82, 0x88010101, 0x02fc8400, 0x820101fe, 0x80008e3f, 0x7e7e01fe, 0x00b97f80, 0xf8f0c003,
        // 0x820080fc, 0x810080ff, 0x853f007f, 0x80fc0000, 0x027f81fe, 0x8201030f, 0x003002f0, 0x013f8000,
        // 0x00851f1f, 0xfe80ff00, 0xf0f8fc06, 0x838080c0, 0x1f82ff82, 0x030f0f02, 0xf88200a8, 0x03800080,
        // 0x80010101, 0x00f88100, 0x000080fc, 0x84018103, 0x80fe00fc, 0x80ff8201, 0x80ff82fc, 0x80008201,
        // 0x7e7e01fe, 0x00857f80, 0x7e80fe00, 0x00a87f81, 0x0080f882, 0x01010380, 0x84008d01, 0x81fe00fc,
        // 0x80009101, 0x7e7e01fe, 0x00997f80, 0xc0008080, 0x3f000081, 0x008d1f80, 0xe001c083, 0x821f80e0,
        // 0x81008d0f, 0x01f081e0, 0x07830f0f, 0xf002008d, 0xff81f8f0, 0x03817f00, 0xb2010101, 0x80f88200,
        // 0xc3830400, 0x80f1e1c3, 0x7fff0400, 0x850f1f3f, 0x0bfc8400, 0xfffdf9fe, 0xffff7fff, 0x010307fe,
        // 0x01000081, 0xfe800085, 0x807e7e01, 0xfcfc0f7f, 0xe0f0f0f8, 0x030380c0, 0x1f0f0f07, 0x00a83f3f,
        // 0x0080f882, 0x80030301, 0x84008d01, 0x81fe00fc, 0x04009101, 0xfe7efefe, 0x80ff80fe, 0x80ff8200,
        // 0xa8018200, 0x80800000, 0x80e000c0, 0x3f3f0100, 0x00807f80, 0xc0808004, 0x0080e0e0, 0x7f023f81,
        // 0xf080e0e0, 0x7f00f880, 0xfb08ff81, 0xf0f0f9fb, 0xfdfcf8f8, 0x7e807f84, 0xfe08fc80, 0x7f7f7efe,
        // 0xf0f0f1f1, 0x3f07e081, 0x0f1f1f3f, 0x85030707, 0x8200a07e, 0x040080f8, 0x0f070703, 0x8000801f,
        // 0xfcfc01f8, 0x01820080, 0xfe07fc84, 0x7f7f3f1f, 0x82fefeff, 0x80fe00fc, 0x010101ff, 0xfe800083,
        // 0x807e7e01, 0xf8f8077f, 0xc0e0e0f0, 0x7f848080, 0x00a93f00, 0xf8f0c003, 0x820080fc, 0x040080ff,
        // 0x7f3f1f07, 0x0000857f, 0x81fe80fc, 0x0187027f, 0x02008201, 0x80fefeff, 0xfefe01fc, 0x01010080,
        // 0x00008001, 0x06fe80ff, 0xc0f0f8fc, 0x84c30000, 0x7f7f05ff, 0x01071f3f, 0xf88200a8, 0xff810080,
        // 0x0080f100, 0x07030104, 0x00850f0f, 0xfe00fc84, 0xf101e180, 0x81ff80fe, 0x0707030f, 0x00850103,
        // 0x7e01fe80, 0x997f807e, 0xf0c00300, 0x0080f8f8, 0x0080ff82, 0x3f1f0704, 0x00857f7f, 0xfe80fc00,
        // 0x87027f81, 0x00820101, 0xfefeff02, 0xfe01fc80, 0x050081fe, 0x00000101, 0xfe80ff7f, 0xe0f8fc05,
        // 0x82810000, 0xfffe07ff, 0x1f3f7f7f, 0x0085070f, 0x8080c002, 0xff810082, 0xe0f8fc0b, 0x07010080,
        // 0x070f1f3f, 0x8200a803, 0x800080f8, 0xe1fd01ff, 0x03010080, 0x850f8007, 0x04fc8400, 0xe1c1c1fe,
        // 0x80ff81fd, 0x0f0f041f, 0x85010107, 0xfefe0100, 0x7f807e80, 0xf8f8fc0f, 0xe0e0f0f0, 0x070303c0,
        // 0x1f0f0f07, 0x0400a81f, 0xfcf8f0e0, 0x000080fc, 0x81ff817f, 0x01010100, 0xfc800087, 0xe0f0f80c,
        // 0x01e180c0, 0x1f0f0703, 0x008e7f3f, 0x810e0201, 0x7ffc02ff, 0x017f817e, 0x00b00f3f, 0x80fefe01,
        // 0x820080ff, 0x820080ff, 0x8000851f, 0x00c08280, 0x001f833f, 0x01008d0f, 0xe083c0c0, 0x07010f83,
        // 0x8100b007, 0x80fe00fc, 0x01010100, 0xfc800083, 0x85fefe01, 0x80fe8000, 0x7f7f017e, 0xfe030085,
        // 0x817e7efe, 0x0100857f, 0xfe807f7f, 0xe0f8fc05, 0x82c18000, 0x043f80ff, 0x070f1f1f, 0x0400a801,
        // 0x7e7f3f3f, 0x0400807e, 0xf8f0f0e0, 0x040080f8, 0x0307070f, 0x02008503, 0x84fe7e7e, 0xfefe07fc,
        // 0x3f3f7f7f, 0x008b0101, 0xf081f881, 0x0f0f1f07, 0x03030707, 0x0000b001, 0x807e817f, 0xe0e00400,
        // 0x80f8f0f0, 0x00078100, 0x0500800f, 0x7ffefefc, 0xfc81fe7f, 0xfc0af881, 0xfffefefc, 0x8f8f3fbf,
        // 0xff81dfcf, 0x1f3f3f09, 0x070f0f1f, 0x81f8f807, 0xe0e009f0, 0x0f0f1f1f, 0x01030707, 0xfe08ff80,
        // 0x3c7c7efe, 0x01010303, 0xf80400a4, 0xc0e0e0f0, 0x07040080, 0xbf1f0f0f, 0xfc040080, 0x7ffffefe,
        // 0x07060080, 0x00000103, 0x00828080, 0xff818000, 0xfffefe0b, 0x0f1f3fff, 0x0303070f, 0x06008507,
        // 0xf0e0c080, 0x81fefcf8, 0xc7ef0cff, 0x07018383, 0x3f1f0f0f, 0xa8ff7f3f, 0x3f3f0400, 0x807e7e7f,
        // 0xf8f00400, 0x80fefcf8, 0x070f0400, 0x85010103, 0x80fe0000, 0xf8f80bfc, 0x7ffef0f0, 0x1f1f3f7f,
        // 0x008d070f, 0xf881f081, 0x83070701, 0x8000b003, 0xf0f001e0, 0xff820080, 0xff800080, 0x803f7f01,
        // 0x01030100, 0x800e0087, 0xf8f0e0c0, 0x7ffffefc, 0x070f0f1f, 0x00870103, 0xe0c0800a, 0xfefcf8f0,
        // 0x0f1f3fff, 0x0080ff82, 0x00893f82, 0x0081f081, 0x008d3f81, 0xf883f000, 0x833ffc01, 0x82008e01,
        // 0x7e7c02fc, 0x8000957e, 0xfefe017e, 0x0080ff80, 0x00b00382, 0x3e3f1f04, 0x00957c7e, 0xf8fc7c07,
        // 0xe0f0f0f8, 0x800080e0, 0x03030101, 0xe002008d, 0x8080c0c0, 0x80000001, 0x0f0f0407, 0x913f1f1f,
        // 0x81f08100, 0x8d3f8100, 0x00f00200, 0x01808200, 0x1f833f3f, 0x8001008d, 0x00c08380, 0x000f831f,
        // 0x80008d07, 0x01fe80e0, 0x0783ffff, 0xaf030301, 0xc0c00d00, 0xf0f0e0e0, 0x1f0f0000, 0x7f3f3f1f,
        // 0xf80b008d, 0xfefcfcf8, 0x7f007f7e, 0x80fcfdff, 0x030081f8, 0x03030101, 0x008a00ff, 0x83ffff01,
        // 0xffff0100, 0x03010083, 0x0400a603, 0x7c7c3e3f, 0x9500fff8, 0xf8c00700, 0xfffefefc, 0xff823f7f,
        // 0xfcfcf802, 0x00860184, 0xff033f80, 0x80fefeff, 0x80fe00fc, 0x7f7f01ff, 0xf88200b0, 0x03010080,
        // 0x8d018003, 0xf8f80100, 0xff82fc83, 0xf0f0f104, 0x03800100, 0x00850780, 0x7efefe02, 0xf002fe82,
        // 0xff80f8f0, 0x073f7f07, 0x01030307, 0x0800c701, 0xfefcf8e0, 0x3f7ffffe, 0x007f81ff, 0x0b008f60,
        // 0xff7f3f3f, 0xf0fcfefe, 0x7f700000, 0x00b83f81, 0x0080f082, 0x80070701, 0x07008503, 0xfefcf8e0,
        // 0x7f7ffffe, 0xf902ff82, 0x0380f8f8, 0x00850182, 0xff7f3f07, 0xfcfefeff, 0x82fc80f0, 0x010101ff,
        // 0xf00f00cb, 0xfefefcf8, 0x1fffff7f, 0xffff7f3f, 0x8dfffffc, 0x3fff0f00, 0xfeffff3f, 0x00fff8fc,
        // 0x3f7f7f60, 0x00911f3f, 0xe0c08003, 0x820080e0, 0x01008dff, 0xfe80e0e0, 0xf0ffff04, 0x3f82070f,
        // 0x008d0300, 0x83f0f001, 0x820380f8, 0x85008d01, 0x010101fc, 0xf00100b4, 0x00fc82f8, 0x03ff8100,
        // 0x00fcf8fc, 0x01800781, 0xfc070085, 0xfcfcf8f8, 0x80fcfefe, 0x013f05ff, 0x017f1f01, 0xfc03008c,
        // 0x803ffffe, 0x80fc00ff, 0xfefc05ff, 0x003fffff, 0x00ab0181, 0xf881f000, 0x03010080, 0x8d018003,
        // 0xf8f80100, 0xfd00fc83, 0xf303ff81, 0x8401f0f1, 0x02008503, 0x81fefefc, 0xf07f027e, 0x81f883f0,
        // 0xa8018103, 0x00f88100, 0x01008170, 0x008f0101, 0xfe80fc80, 0x957e7e01, 0x7e7e0100, 0x3f807f80,
        // 0x80810099, 0x0f030081, 0x8e0f1f1f, 0x80c08100, 0x830700e0, 0x8d07000f, 0x81e08100, 0x010783f0,
        // 0x008d0303, 0xf8f0f007, 0xfffffefe, 0x0103817f, 0x00b20101, 0xfc81f800, 0x01800080, 0xfc80008f,
        // 0x807efe01, 0xfcf80afe, 0x3f7ffffe, 0x03071f3f, 0x02008a01, 0x82fffffe, 0x1f1f073f, 0xfeff7f3f,
        // 0x0083fcfc, 0xa8030101, 0x01f88000, 0x0080fcfc, 0x008d0182, 0xfe80fc81, 0x00957e00, 0x7f017e80,
        // 0xd53f807f, 0xf8f80100, 0xff82fc83, 0xf8f8fb03, 0x04ff81fc, 0x01e1e3e7, 0x02078303, 0x81fefefc,
        // 0x817f007e, 0x83fc81f8, 0xf8f003f1, 0x03830707, 0xf80100bd, 0x82fc83f8, 0xf0f103ff, 0x038401f0,
        // 0xfc020085, 0x7e81fefe, 0xf0f07f02, 0x0381f883, 0x00c50181, 0xfcf8e009, 0x7ffffefe, 0x807f3f3f,
        // 0xf8fc02ff, 0x800080f8, 0x03030101, 0x3f100085, 0xfeff7f3f, 0xf8f0fcfe, 0xfffffcf8, 0x031f3f7f,
        // 0x008f0180, 0x83e0e001, 0x07070100, 0xe080008d, 0xf8f0f005, 0x8207fefc, 0xc3e305ff, 0x07030100,
        // 0x00850f81, 0xfffffe02, 0xc303f882, 0x81f1e3c3, 0x030f81ff, 0x01030707, 0xfc840085, 0x01fffe02,
        // 0xe00700b4, 0xfefefcf8, 0x82007fff, 0xfcfc02ff, 0x86018300, 0x003f8000, 0x00ff807f, 0x03fc81fe,
        // 0x7f7fffff, 0xfc00008d, 0x7f010084, 0x807f807e, 0x0100cd3f, 0xfe81fcfc, 0x817e7e01, 0x1c1f023f,
        // 0x01008e08, 0x3f837f7e, 0xf80100d5, 0x01fe82fc, 0x1f810ffc, 0x03011802, 0xf802008d, 0xff81c3f0,
        // 0x8207fe01, 0x0307010f, 0xf08000b2, 0x07020082, 0x008d0303, 0x80fefe01, 0xf8f802ff, 0x001f81fc,
        // 0x8d01800f, 0x04fc8300, 0x0001f0f8, 0xcd0f820c, 0xfefe0100, 0x7f027e81, 0xf884f07f, 0x01820380,
        // 0x7f020085, 0xff803f3f, 0xfcfefe04, 0xff82fefc, 0xcb010101, 0x801f0000, 0x803e003f, 0xfcfc08fe,
        // 0x3f3f7efe, 0x8c011f1f, 0x81fc8100, 0x0f0f06f8, 0x03030707, 0x0100ce01, 0x3f801f1f, 0xfefebe05,
        // 0x817e3e3c, 0xf8ff0a7f, 0x7efefcfc, 0x011f3f3f, 0x00008301, 0x80fc81fe, 0x0cff80f8, 0xf1f1fbfb,
        // 0x0f0f1ff0, 0x01030707, 0x0a00c501, 0xf0f0f8f8, 0x80c0c0e0, 0x80fbf3e1, 0x3f7f06ff, 0x03070f0f,
        // 0x0f008801, 0xf8f0e0c0, 0x7ffefcfc, 0xff7f7f3f, 0xf8f8fdff, 0x01020082, 0x00a60303, 0x803f3f01,
        // 0xfcfc0e7e, 0xf8f8f000, 0x7efefcfc, 0x01030300, 0x02008801, 0x80f8f8fc, 0xe0e009f0, 0x1f3f3f7f,
        // 0x070f0f1f, 0xf00c008d, 0xfcf8f8f0, 0x077efefc, 0x01010303, 0xfc8100d0, 0xc0800003, 0x06ff80e0,
        // 0x1f3f7f7f, 0x8b01010f, 0xf0e00c00, 0xfefcf8f8, 0x070fffff, 0x80ff0303, 0x0300917f, 0xf8f8f0e0,
        // 0x0f810081, 0xf882008d, 0xfffffc03, 0x8e01830f, 0x7fff0400, 0x80fcffff, 0x810081f8, 0x83008d01,
        // 0xe0f001f8, 0x0f820180, 0x1f810091, 0x1f850095, 0x1f850095, 0x1f850095, 0x7f000099, 0x0095ff80,
        // 0x83fcff01, 0x010183f8, 0x008d0f07, 0xf882f080, 0x03000f81, 0x008d0180, 0xfcf8f802, 0x7f02ff81,
        // 0x00d40101, 0x81fef801, 0x000303ff, 0xff828700, 0x82020001, 0xc7010003, 0xffff0d00, 0x3b1f1f0f,
        // 0xffff0000, 0xdcf8f8f0, 0x7b0b008d, 0xc3e3e373, 0xdee3e3c3, 0x80c7c7ce, 0x8dc700c3, 0x73f30f00,
        // 0x0f1f3f3b, 0xcecfffff, 0xf0f8fcdc, 0x0000ffff, 0x04010080, 0x00000000, 0xe1a00000, 0xe1a00000,
        // 0x010a0009, 0x01150212, 0x011a0113, 0x020a0119, 0xff0c020d, 0x02150212, 0x010fff0c, 0xfe14000a,
        // 0x05130113, 0x01130013, 0x01130013, 0x02130113, 0x01130013, 0xff0c000a, 0x02150215, 0x010e0215,
        // 0x0016011f, 0x01160116, 0x01140119, 0x01180112, 0x010b011b, 0x0119fe0e, 0x00210112, 0x011a011b,
        // 0x011a0115, 0x00110117, 0x021b0217, 0x01220216, 0x0216ff1b, 0x000eff1a, 0xfe0f010f, 0x00120215,
        // 0x0113030b, 0x01110013, 0x01120114, 0x00130010, 0x010a0013, 0x0114fe0d, 0x001c010a, 0x01130013,
        // 0x01130015, 0x000d010f, 0x0113000d, 0x011a0112, 0x0012fe15, 0x010d0011, 0x010d0309, 0x00100215,
        // 0x7e787eb9, 0x7e367e57, 0x7dd57e15, 0x7d937db4, 0x7d317d72, 0x7cf07d11, 0x788e7caf, 0x784d786e,
        // 0x00000000, 0x00000000, 0xf0000000, 0xff000000, 0xeef00000, 0xdeef0000, 0xcdeff000, 0xccdef000,
        // 0xfffff000, 0xeeeeeff0, 0xdddddeef, 0xccccddde, 0xbbbbccdd, 0xaaaabbcc, 0x9999aabc, 0x88889aab,
        // 0xbcddef00, 0xabcdef00, 0xabcddef0, 0x9abcdef0, 0x9abcdef0, 0x9abcdef0, 0x9abcdef0, 0x9abcdef0,
        // 0x7778899a, 0x6667789a, 0x55567789, 0x44556788, 0x33455678, 0x22345678, 0x12345678, 0x12345678,
        // 0xf8f8f8f8, 0xf8f8f8f8, 0x00ffffff, 0x7f000000, 0x7c7d7d7d, 0x7c7c7c7c, 0x00000000, 0x1e1cf8e0,
        // 0x00000000, 0xdecfc700, 0x00000000, 0x8fcfff00, 0x00000000, 0xf7e7c300, 0x00000000, 0xe1f1ff1e,
        // 0x00000000, 0xf1f1f8f8, 0x00000000, 0xf1f1e0e0, 0x00f0f0f0, 0xf1f1f303, 0x00010101, 0xf9f9f9f8,
        // 0x00000000, 0xe0e0e0e0, 0x00000000, 0xc3e3f3f3, 0x00000000, 0xfff3e1e1, 0x00000000, 0x00010103,
        // 0xf8f8f8f8, 0xf8f8f8f8, 0x0000007f, 0x00000000, 0x7c7c7c7c, 0x7c7c7c7c, 0x1f1fff1e, 0xf83c1e1e,
        // 0xc0c0dfde, 0xc7cfdede, 0x0f0f0f0f, 0x0f0f0f0f, 0xf0f0f0f0, 0x80e0e0f0, 0xe1e1e1e1, 0x7ff3e1e1,
        // 0xc3e3e3f3, 0x808081c3, 0x7b7bfbf1, 0x1f1f3f3f, 0xf0f0f0f0, 0xf0f0f0f0, 0xf9f9f9f9, 0xe1f1f9f9,
        // 0xe0e0e0e0, 0xffe1e0e0, 0x83030383, 0xf3f3e3c3, 0x7f3f3f7f, 0xe1f1f3ff, 0x00000000, 0x03010100,
        // 0x00000000, 0x00000000, 0x00000000, 0xfc000000, 0x00000000, 0x1f000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0xf8000000,
        // 0x00000000, 0x1f000000, 0x00000000, 0xfc000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x7c000000, 0x00000000, 0x80000000, 0x00000000, 0x0f000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x7c7cfcfc, 0x7c7c7c7c, 0xf8f8ff7f, 0xf8f8f8f8, 0x01000000, 0x7979f1c1,
        // 0x00000000, 0x783c1f07, 0x00000000, 0x1f3fdf9f, 0x00000000, 0x7c7e7f1f, 0x3f3efefc, 0xf8fcfeff,
        // 0x00383f3f, 0xff7f1f01, 0x0000fcfc, 0xfcfcfcfc, 0x00000000, 0x7cfcfc7c, 0x00000000, 0xf8f8ff7f,
        // 0x7c7c7c7c, 0xfd7c7c7c, 0x80808080, 0xff808080, 0x0f0f0f0f, 0x0f0f0f0f, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x7c7c7c7c, 0xfcfcfcfc, 0xf8f8f8f8, 0x0f3f7fff, 0x7d7dfdfd, 0xe0f07878,
        // 0x00007f7f, 0x0f3c7878, 0x1f1f1f1f, 0x1f1f1f1f, 0x7c7c7c7c, 0x7c7c7c7c, 0x0e0200c0, 0xf0fcfefe,
        // 0xf8f8fcff, 0x1f7fffff, 0xfdfdfdfd, 0xfcfcfcfd, 0x7c7c7c7c, 0x7c7c7c7c, 0xf0f0f0f0, 0xf0f0f0f0,
        // 0x7d7d7dfd, 0x7d7d7d7d, 0x808080ff, 0x80808080, 0x0f0f0f0f, 0x0f0f0f0f, 0x00000000, 0x00000000,
        // 0x04010100, 0x80000001, 0xe1a00000, 0xe1a00000, 0x04010180, 0x80000003, 0xe1a00000, 0xe1a00000,
        // 0x55432008, 0x472d544c, 0x00004142, 0xe1a00000, 0xe92d4000, 0xe59f0114, 0xe3a01402, 0xe92d001c,
        // 0xe4902004, 0xe1a02422, 0xebfff7ab, 0xe3a00405, 0xe1df1fbc, 0xe1c010b0, 0xe1df1fb8, 0xe1c010b2,
        // 0xe1c010b6, 0xe1df1fb0, 0xe1c010b4, 0xe1c010b8, 0xe3a00301, 0xe1df1eb4, 0xe1c010b0, 0xe3a01008,
        // 0xe1c010b4, 0xe1df1db8, 0xe1c014ba, 0xe3a01007, 0xe1c010b8, 0xe59f00cc, 0xe59f10cc, 0xe3a02008,
        // 0xe4903004, 0xe4813004, 0xe2522001, 0xcafffffb, 0xe59f10b8, 0xe3a02020, 0xe4903004, 0xe4813004,
        // 0xe2522001, 0xcafffffb, 0xe59f00a4, 0xe59f10a4, 0xe3a02008, 0xe4801004, 0xe2522001, 0xcafffffc,
        // 0xe1a01000, 0xe59f0090, 0xe59f2090, 0xe3a03001, 0xe92d1ffc, 0xe1d230b0, 0xebfff709, 0xe2533001,
        // 0x059f207c, 0x0afffff9, 0xe59fc078, 0xe59fb078, 0xe3a0a040, 0xe4db4002, 0xe3a00000, 0xe3a03000,
        // 0xe7db1003, 0xe06a1081, 0xe19c10f1, 0xe0800441, 0xe20110ff, 0xe0800001, 0xe2833001, 0xe1530004,
        // 0xbafffff6, 0xe1a000a0, 0xe2600078, 0xea00000f, 0x000011a0, 0x00007c23, 0x00007fff, 0x00006c5a,
        // 0x00009140, 0x00003001, 0x00002420, 0x05000220, 0x06017f80, 0x06004000, 0x11111111, 0x000024c0,
        // 0x00002740, 0x00002750, 0x00002360, 0x00002760, 0xe55b1001, 0xe3a03000, 0xe7db2003, 0xeb00003b,
        // 0xe06a5082, 0xe19c50f5, 0xe0800445, 0xe20550ff, 0xe0800005, 0xe2833001, 0xe1530004, 0xbafffff5,
        // 0xe59f306c, 0xe3a04000, 0xe3a05fff, 0xe3a06901, 0xe1a02001, 0xe3140002, 0x12822010, 0xe0c320b2,
        // 0xe1860604, 0xe3140001, 0x12800010, 0xe0c300b2, 0xe1df04b0, 0xe0c300b4, 0xe2844001, 0xe3540004,
        // 0xbafffff2, 0xe59f0030, 0xe3a01001, 0xe3a03005, 0xe3a02010, 0xe0c010b2, 0xe2811001, 0xe2522001,
        // 0xcafffffb, 0xe2800020, 0xe2533001, 0xcafffff7, 0xea000002, 0x070003e0, 0x000017fc, 0x0600038e,
        // 0xe3a080f0, 0xe59fa03c, 0xe3a0b001, 0xe0cab0b2, 0xebfff899, 0xe3a0b001, 0xe1cab0b0, 0xe59f3028,
        // 0xe3a04004, 0xe1d310b0, 0xe2811004, 0xe0c310b8, 0xe2544001, 0xcafffffa, 0xe2588004, 0xcafffff3,
        // 0xe8bd4000, 0xe12fff1e, 0x04000200, 0x070003e2, 0xe92d400f, 0xe92d0003, 0xe3a00402, 0xe2422020,
        // 0xe0800382, 0xe59f1034, 0xe0811483, 0xe59f2030, 0xebfff696, 0xe8bd0003, 0xe3a02407, 0xe0822183,
        // 0xe3811b02, 0xe0c210b2, 0xe3800902, 0xe0c200b2, 0xe1a03203, 0xe0c230b2, 0xe8bd400f, 0xe12fff1e,
        // 0x06010000, 0x00002350, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
        // 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
    };
    var instr_strs = std.ArrayList(u8).init(std.testing.allocator);
    for (instrs) |instr, i| {
        const instruction = Instruction.parse(instr, @intCast(u32, i) * 4);
        try std.fmt.format(instr_strs.writer(), "{}\n", .{instruction});
    }

    defer instr_strs.deinit();
    try std.testing.expectEqualStrings(
        \\b 0x20
        \\b 0x174
        \\b 0x128
        \\b 0x178
        \\b 0x178
        \\b 0x178
        \\b 0x110
        \\b 0x178
        \\mov r0, 0x4000000
        \\strb r0, [r0, 0x8]
        \\msr cpsr_fc, 0x1f
        \\bl 0xcc
        \\mov r0, 0xff
        \\bl 0xca0
        \\bl 0x2770
        \\mov r0, 0xff
        \\bl 0xca0
        \\mov r0, 0x4000000
        \\bl 0xcc
        \\mov r0, 0x4000000
        \\mov r1, 0x98
        \\ldrh r2, [r1], 0x2
        \\ldrh r3, [r1], 0x2
        \\strh r3, [r0, r2]
        \\cmp r1, 0xb8
        \\blt 0x54
        \\strh r0, [r0, 0x4]
        \\strh r0, [r0, 0x8]
        \\strh r0, [r0, 0x4a]
        \\add r1, r0, 0x208
        \\strh r0, [r1]
        \\msr cpsr_fc, 0x1f
        \\mov r0, 0x0
        \\mov r1, 0x0
        \\mov r2, 0x0
        \\mov r3, 0x0
        \\mov lr, 0x8000000
        \\bx lr
        \\addeq r0, r0, r0
        \\tsteq r0, r0, lsr 0x20
        \\tsteq r0, r6, lsr 0x20
        \\tsteq r0, r0, lsr r0
        \\tsteq r0, r6, lsr r0
        \\stmdahi lr, {r1, r7}
        \\andeq r0, r0, 0x88
        \\andhi r0, r0, r4, lsr r1
        \\mov r0, 0x4000000
        \\ldrb r2, [r0, -0x6]
        \\tst r2, 0x1
        \\moveq lr, 0x8000000
        \\movne lr, 0x2000000
        \\msr cpsr_fc, 0x13
        \\ldr sp, [pc, 0x1068]
        \\mov lr, 0x0
        \\msr spsr_fc, lr
        \\msr cpsr_c, 0x12
        \\ldr sp, [pc, 0x106c]
        \\mov lr, 0x0
        \\msr spsr_fc, lr
        \\msr cpsr_fc, 0x1f
        \\ldr sp, [pc, 0x1070]
        \\ldr r1, [pc, 0x1074]
        \\mov r2, 0x0
        \\str r2, [r0, r1]
        \\adds r1, r1, 0x4
        \\bne 0xfc
        \\ldmda r0, {r0, r1, r2, r3, r4, r5, r6, r7, r8, r9, sl, fp, ip}
        \\bx lr
        \\push {r0, r1, r2, r3, ip, lr}
        \\mov r0, 0x4000000
        \\add lr, pc, 0x0
        \\ldr pc, [r0, -0x4]
        \\pop {r0, r1, r2, r3, ip, lr}
        \\subs pc, lr, 0x4
        \\push {fp, ip, lr}
        \\ldrb ip, [lr, -0x2]
        \\add fp, pc, 0x44
        \\ldr ip, [fp, ip, lsl 0x2]
        \\mrs fp, spsr
        \\stmfd sp!, {fp}
        \\and fp, fp, 0x80
        \\orr fp, fp, 0x1f
        \\msr cpsr_fc, fp
        \\push {r2, lr}
        \\add lr, pc, 0x0
        \\bx ip
        \\pop {r2, lr}
        \\mov ip, 0x93
        \\msr cpsr_fc, ip
        \\ldmfd sp!, {fp}
        \\msr spsr_fc, fp
        \\pop {fp, ip, lr}
        \\movs pc, lr
        \\b 0x174
        \\b 0x178
        \\strheq r0, [r0], -r8
        \\andeq r0, r0, r0, lsr 0x19
        \\undef
        \\andeq r0, r0, ip, lsl 0x18
        \\andeq r0, r0, r0, lsr ip
        \\andeq r0, r0, r8, lsr 0x18
        \\andeq r0, r0, r4, lsr r7
        \\andeq r0, r0, r8, lsr 0xe
        \\muleq r0, r8, r7
        \\andeq r0, r0, r0, asr 0x10
        \\andeq r0, r0, ip, ror 0xf
        \\andeq r0, r0, r4, lsr r2
        \\andeq r0, r0, r4, asr 0x5
        \\andeq r0, r0, ip, lsr 0x4
        \\andeq r0, r0, r8, asr fp
        \\andeq r0, r0, r0, ror 0x15
        \\andeq r0, r0, r0, asr r4
        \\andeq r0, r0, r8, lsr 0x6
        \\andeq r0, r0, ip, lsr 0x7
        \\andeq r0, r0, ip, lsl 0xb
        \\andeq r0, r0, r4, lsr 0xc
        \\muleq r0, r0, r6
        \\undef
        \\andeq r0, r0, r8, lsl #10
        \\andeq r0, r0, r4, asr r5
        \\andeq r0, r0, r8, lsr 0x4
        \\andeq r0, r0, ip, lsr #31
        \\andeq r0, r0, r8, lsr 0x4
        \\andeq r0, r0, r8, lsr 0x4
        \\andeq r1, r0, r0, asr 0x2
        \\andeq r0, r0, r8, lsr 0x4
        \\andeq r0, r0, r4, lsr #29
        \\andeq r0, r0, r8, lsr 0x4
        \\andeq r0, r0, r8, lsr 0x4
        \\andeq r0, r0, r8, lsr 0x4
        \\andeq r0, r0, r8, lsr 0x4
        \\andeq r0, r0, r8, lsr 0x4
        \\andeq r0, r0, r8, lsr 0x4
        \\andeq r0, r0, r0, lsr #32
        \\andeq r0, r0, ip, lsl ip
        \\undef
        \\andeq r1, r0, r0, asr 0x1
        \\andeq r0, r0, r8, lsl #30
        \\bx lr
        \\ldr r0, [pc, 0x1078]
        \\bx lr
        \\push {r3, r4}
        \\lsl r3, r2, #11
        \\lsrs r3, r3, #11
        \\beq 0x2bc
        \\lsrs r2, r2, #25
        \\orrcs r2, r2, 0x1
        \\add r4, pc, 0x0
        \\ldr pc, [r4, r2, lsl #2]
        \\andeq r0, r0, r4, ror 0x4
        \\andeq r0, r0, ip, ror r2
        \\muleq r0, r4, r2
        \\andeq r0, r0, ip, lsr 0x5
        \\ldrh r4, [r0], 0x2
        \\strh r4, [r1], 0x2
        \\subs r3, r3, 0x1
        \\bne 0x264
        \\pop {r3, r4}
        \\bx lr
        \\ldrh r4, [r0]
        \\strh r4, [r1], 0x2
        \\subs r3, r3, 0x1
        \\bne 0x280
        \\pop {r3, r4}
        \\bx lr
        \\ldm r0!, {r4}
        \\stmia r1!, {r4}
        \\subs r3, r3, 0x1
        \\bne 0x294
        \\pop {r3, r4}
        \\bx lr
        \\ldr r4, [r0]
        \\str r4, [r1], 0x4
        \\subs r3, r3, 0x1
        \\bne 0x2b0
        \\pop {r3, r4}
        \\bx lr
        \\push {r3, r4, r5, r6, r7, r8, r9, sl, fp}
        \\lsl r3, r2, #11
        \\lsrs r3, r3, #11
        \\beq 0x320
        \\tst r2, 0x1000000
        \\bne 0x2f4
        \\ldm r0!, {r4, r5, r6, r7, r8, r9, sl, fp}
        \\stmia r1!, {r4, r5, r6, r7, r8, r9, sl, fp}
        \\subs r3, r3, 0x8
        \\bgt 0x2dc
        \\pop {r3, r4, r5, r6, r7, r8, r9, sl, fp}
        \\bx lr
        \\ldr r4, [r0]
        \\mov r5, r4
        \\mov r6, r4
        \\mov r7, r4
        \\mov r8, r4
        \\mov r9, r4
        \\mov sl, r4
        \\mov fp, r4
        \\stmia r1!, {r4, r5, r6, r7, r8, r9, sl, fp}
        \\subs r3, r3, 0x8
        \\bgt 0x314
        \\pop {r3, r4, r5, r6, r7, r8, r9, sl, fp}
        \\bx lr
        \\push {r3, r4, r5, r6}
        \\ldr r2, [r0], 0x4
        \\lsrs r2, r2, 0x8
        \\beq 0x3a4
        \\ldrb r3, [r0], 0x1
        \\orr r3, r3, 0x1000000
        \\tst r3, 0x80
        \\bne 0x364
        \\ldrb r4, [r0], 0x1
        \\strb r4, [r1], 0x1
        \\subs r2, r2, 0x1
        \\beq 0x3a4
        \\lsls r3, r3, 0x1
        \\bcc 0x340
        \\b 0x338
        \\ldrb r4, [r0], 0x1
        \\ldrb r5, [r0], 0x1
        \\orr r5, r5, r4, lsl 0x8
        \\bic r5, r5, 0xf000
        \\add r5, r5, 0x1
        \\lsr r4, r4, 0x4
        \\add r4, r4, 0x3
        \\ldrb r6, [r1, -r5]
        \\strb r6, [r1], 0x1
        \\subs r2, r2, 0x1
        \\beq 0x3a4
        \\subs r4, r4, 0x1
        \\bne 0x380
        \\lsls r3, r3, 0x1
        \\bcc 0x340
        \\b 0x338
        \\pop {r3, r4, r5, r6}
        \\bx lr
        \\push {r3, r4, r5, r6, r7}
        \\ldr r2, [r0], 0x4
        \\lsrs r2, r2, 0x8
        \\beq 0x448
        \\ldrb r3, [r0], 0x1
        \\orr r3, r3, 0x1000000
        \\tst r3, 0x80
        \\bne 0x3f8
        \\ldrb r4, [r0], 0x1
        \\tst r1, 0x1
        \\moveq r7, r4
        \\orrne r7, r7, r4, lsl 0x8
        \\strhne r7, [r1]
        \\add r1, r1, 0x1
        \\subs r2, r2, 0x1
        \\beq 0x448
        \\lsls r3, r3, 0x1
        \\bcc 0x3c4
        \\b 0x3bc
        \\ldrb r4, [r0], 0x1
        \\ldrb r5, [r0], 0x1
        \\orr r5, r5, r4, lsl 0x8
        \\bic r5, r5, 0xf000
        \\add r5, r5, 0x1
        \\lsr r4, r4, 0x4
        \\add r4, r4, 0x3
        \\ldrb r6, [r1, -r5]
        \\tst r1, 0x1
        \\moveq r7, r6
        \\orrne r7, r7, r6, lsl 0x8
        \\strhne r7, [r1]
        \\add r1, r1, 0x1
        \\subs r2, r2, 0x1
        \\beq 0x448
        \\subs r4, r4, 0x1
        \\bne 0x414
        \\lsls r3, r3, 0x1
        \\bcc 0x3c4
        \\b 0x3bc
        \\pop {r3, r4, r5, r6, r7}
        \\bx lr
        \\push {r2, r3, r4, r5, r6, r7, r8, r9, sl, fp, ip}
        \\ldrh r3, [r2]
        \\cmp r3, 0x0
        \\lsrsne r4, r0, #25
        \\beq 0x4c8
        \\ldrb r4, [r2, #2]
        \\ldrb r5, [r2, #3]
        \\ldr r2, [r2, #4]
        \\lsr r6, r2, #31
        \\bic r2, r2, 0x80000000
        \\mov ip, 0x1
        \\rsb ip, ip, ip, lsl r4
        \\mov r7, 0x0
        \\mov r9, 0x0
        \\subs r3, r3, 0x1
        \\bmi 0x4c8
        \\ldrb fp, [r0], 0x1
        \\mov r8, 0x0
        \\cmp r8, 0x8
        \\bge 0x488
        \\ands sl, ip, fp, lsr r8
        \\cmpeq r6, 0x0
        \\addne sl, sl, r2
        \\orr r7, r7, sl, lsl r9
        \\add r9, r9, r5
        \\ands r9, r9, #31
        \\streq r7, [r1], 0x4
        \\moveq r7, 0x0
        \\add r8, r8, r4
        \\b 0x498
        \\pop {r2, r3, r4, r5, r6, r7, r8, r9, sl, fp, ip}
        \\bx lr
        \\push {r2, r3, r4}
        \\ldr r2, [r0], 0x4
        \\lsr r2, r2, 0x8
        \\cmp r2, 0x0
        \\lsrsne r3, r0, #25
        \\beq 0x500
        \\ldrb r3, [r0], 0x1
        \\strb r3, [r1], 0x1
        \\ldrb r4, [r0], 0x1
        \\add r3, r3, r4
        \\subs r2, r2, 0x1
        \\bgt 0x4ec
        \\pop {r2, r3, r4}
        \\bx lr
        \\push {r2, r3, r4, r5}
        \\ldr r2, [r0], 0x4
        \\lsr r2, r2, 0x8
        \\cmp r2, 0x0
        \\lsrsne r3, r0, #25
        \\beq 0x54c
        \\ldrb r3, [r0], 0x1
        \\ldrb r4, [r0], 0x1
        \\orr r4, r3, r4, lsl 0x8
        \\add r4, r4, r3, lsl 0x8
        \\strh r4, [r1], 0x2
        \\ldrb r3, [r0], 0x1
        \\add r3, r3, r4, lsr 0x8
        \\and r3, r3, 0xff
        \\ldrb r4, [r0], 0x1
        \\subs r2, r2, 0x2
        \\bgt 0x528
        \\pop {r2, r3, r4, r5}
        \\bx lr
        \\push {r2, r3, r4}
        \\ldr r2, [r0], 0x4
        \\lsr r2, r2, 0x8
        \\cmp r2, 0x0
        \\lsrsne r3, r0, #25
        \\beq 0x584
        \\ldrh r3, [r0], 0x2
        \\strh r3, [r1], 0x2
        \\ldrh r4, [r0], 0x2
        \\add r3, r3, r4
        \\subs r2, r2, 0x2
        \\bgt 0x570
        \\pop {r2, r3, r4}
        \\bx lr
        \\push {r2, r3, r4, r5, r6, r7, r8, r9, sl, fp}
        \\ldr r3, [r0], 0x4
        \\lsrs r2, r3, 0x8
        \\lsrsne r2, r0, #25
        \\beq 0x61c
        \\and r2, r3, #15
        \\lsr r3, r3, 0x8
        \\ldrb r4, [r0], 0x1
        \\add r4, r0, r4, lsl 0x1
        \\add r4, r4, 0x1
        \\mov r5, 0x0
        \\mov r6, 0x0
        \\mov r7, r0
        \\mov r8, #32
        \\ldr r9, [r4], 0x4
        \\subs r8, r8, 0x1
        \\blt 0x5c0
        \\ldrb sl, [r7]
        \\and fp, sl, 0x3f
        \\add fp, fp, 0x1
        \\bic r7, r7, 0x1
        \\lsls r9, r9, 0x1
        \\adc r7, r7, fp, lsl 0x1
        \\lslcs sl, sl, 0x1
        \\tst sl, 0x80
        \\beq 0x5c8
        \\ldrb r7, [r7]
        \\orr r5, r5, r7, lsl r6
        \\mov r7, r0
        \\add r6, r6, r2
        \\ands r6, r6, #31
        \\bne 0x5c8
        \\str r5, [r1], 0x4
        \\mov r5, 0x0
        \\subs r3, r3, 0x4
        \\bgt 0x5c8
        \\pop {r2, r3, r4, r5, r6, r7, r8, r9, sl, fp}
        \\bx lr
        \\push {r2, r3, r4}
        \\ldr r2, [r0], 0x4
        \\lsr r2, r2, 0x8
        \\cmp r2, 0x0
        \\lsrsne r3, r0, #25
        \\beq 0x688
        \\ldrb r3, [r0], 0x1
        \\lsls r3, r3, #25
        \\lsr r3, r3, #25
        \\bcc 0x668
        \\ldrb r4, [r0], 0x1
        \\add r3, r3, 0x3
        \\sub r2, r2, r3
        \\strb r4, [r1], 0x1
        \\subs r3, r3, 0x1
        \\bgt 0x658
        \\b 0x680
        \\add r3, r3, 0x1
        \\sub r2, r2, r3
        \\ldrb r4, [r0], 0x1
        \\strb r4, [r1], 0x1
        \\subs r3, r3, 0x1
        \\bgt 0x670
        \\cmp r2, 0x0
        \\bgt 0x63c
        \\pop {r2, r3, r4}
        \\bx lr
        \\push {r2, r3, r4, r5, r6}
        \\ldr r2, [r0], 0x4
        \\lsr r2, r2, 0x8
        \\cmp r2, 0x0
        \\lsrsne r3, r0, #25
        \\beq 0x720
        \\mov r5, 0x0
        \\mov r6, 0x0
        \\ldrb r3, [r0], 0x1
        \\lsls r3, r3, #25
        \\lsr r3, r3, #25
        \\bcc 0x6ec
        \\ldrb r4, [r0], 0x1
        \\add r3, r3, 0x3
        \\sub r2, r2, r3
        \\subs r3, r3, 0x1
        \\blt 0x718
        \\orr r6, r6, r4, lsl r5
        \\eors r5, r5, 0x8
        \\bne 0x6cc
        \\strh r6, [r1], 0x2
        \\mov r6, 0x0
        \\b 0x6cc
        \\add r3, r3, 0x1
        \\sub r2, r2, r3
        \\subs r3, r3, 0x1
        \\blt 0x718
        \\ldrb r4, [r0], 0x1
        \\orr r6, r6, r4, lsl r5
        \\eors r5, r5, 0x8
        \\bne 0x6f4
        \\strh r6, [r1], 0x2
        \\mov r6, 0x0
        \\b 0x6f4
        \\cmp r2, 0x0
        \\bgt 0x6b0
        \\pop {r2, r3, r4, r5, r6}
        \\bx lr
        \\mov r3, r1
        \\mov r1, r0
        \\mov r0, r3
        \\push {r2, r4}
        \\movs r4, r1
        \\rsbsmi r1, r1, 0x0
        \\beq 0x794
        \\eors r4, r4, r0, asr #32
        \\rsbcs r0, r0, 0x0
        \\mov r2, 0x1
        \\cmp r1, r0, lsr 0x1
        \\lslls r2, r2, 0x1
        \\lslls r1, r1, 0x1
        \\bls 0x750
        \\mov r3, 0x0
        \\cmp r1, r0
        \\suble r0, r0, r1
        \\addle r3, r3, r2
        \\lsrne r1, r1, 0x1
        \\lsrsne r2, r2, 0x1
        \\bne 0x764
        \\eors r1, r0, r4, asr #32
        \\adc r1, r1, 0x0
        \\eors r0, r3, r4, asr #32
        \\adc r0, r0, 0x0
        \\pop {r2, r4}
        \\bx lr
        \\b 0x78c
        \\push {r1, r2, r3, r4}
        \\mov r1, 0x1
        \\mov r2, r0
        \\cmp r1, r2, lsr 0x2
        \\lslls r1, r1, 0x1
        \\lsrls r2, r2, 0x1
        \\bls 0x7a4
        \\lsr r2, r1, 0x1
        \\add r3, r1, r2
        \\mul r4, r3, r3
        \\cmp r4, r0
        \\addls r1, r1, r2
        \\lsrsne r2, r2, 0x1
        \\bne 0x7b8
        \\mov r0, r1
        \\pop {r1, r2, r3, r4}
        \\bx lr
        \\subeq r0, r0, r0
        \\addeq r0, r0, r0, asr #32
        \\sbceq r0, r0, r0, lsl 0x1
        \\tsteq r0, r0, asr 0x1
        \\push {r2, r4, r5, lr}
        \\mov r2, r0
        \\mov r3, r1
        \\mov r4, 0x0
        \\cmp r1, 0x0
        \\addlt r4, r4, 0x8
        \\rsblt r3, r3, 0x0
        \\eors r5, r1, r0, asr #32
        \\addmi r4, r4, 0x4
        \\rsbcs r2, r2, 0x0
        \\subs r2, r2, r3
        \\movpl r3, r1
        \\movpl r1, r0
        \\movpl r0, r3
        \\eors r5, r5, r2
        \\addmi r4, r4, 0x2
        \\ldr r3, [pc, 0x107c]
        \\ldrh r4, [r3, r4]
        \\lsl r0, r0, #14
        \\bl 0x734
        \\ldr lr, [pc, 0x1080]
        \\mul r1, r0, r0
        \\asr r1, r1, #14
        \\rsb r1, r1, 0x0
        \\mov r3, 0xa9
        \\mul r3, r1, r3
        \\asr r3, r3, #14
        \\add r3, r3, 0x390
        \\mul r3, r1, r3
        \\asr r3, r3, #14
        \\add r3, r3, 0x900
        \\add r3, r3, #28
        \\mul r3, r1, r3
        \\asr r3, r3, #14
        \\add r3, r3, 0xf00
        \\add r3, r3, 0xb6
        \\mul r3, r1, r3
        \\asr r3, r3, #14
        \\add r3, r3, 0x1600
        \\add r3, r3, 0 ; 0xaa
        \\mul r3, r1, r3
        \\asr r3, r3, #14
        \\add r3, r3, 0x2000
        \\add r3, r3, 0x81
        \\mul r3, r1, r3
        \\asr r3, r3, #14
        \\add r3, r3, 0x3600
        \\add r3, r3, 0x51
        \\mul r3, r1, r3
        \\asr r3, r3, #14
        \\add r3, r3, 0xa200
        \\add r3, r3, 0xf9
        \\mul r0, r3, r0
        \\asr r0, r0, #16
        \\bx lr
        \\eors r0, r0, r2, asr #32
        \\adc r0, r0, 0x0
        \\add r0, r0, r4, lsl 0x8
        \\mov r3, 0x170
        \\pop {r2, r4, r5, lr}
        \\bx lr
        \\orrseq r0, r2, r0
        \\ldrteq r0, [r5], 0x323
        \\ldrbeq r0, [r5, r5, asr #12]
        \\beq 0xffc42e84
        \\mcreq 12, 0, r0, cr5, cr12, {3}
        \\tstne r1, ip, lsl #31
        \\ldrne r1, [r3], 0xfffffd6c
        \\strne r1, [r8, -pc, lsl #11]
        \\stmibne pc!, {r0, r2, r3, r4, r5, r6, fp, ip}^ ; <UNPREDICTABLE>
        \\fstmiaxne r6, {d17-d62} ;@ Deprecated
        \\svcne 0x008b1e2b
        \\eorscs r2, sp, 0xe7
        \\ldrbcs r2, [sl], 0x38e
        \\undef
        \\stmibcs sp, {r0, r3, r4, r7, fp, sp}^
        \\undef
        \\cdpcs 13, 5, cr2, cr10, cr1, {2}
        \\rsbscc r2, r6, fp, ror #30
        \\rsbscc r3, r4, 0x4000001e
        \\ldrbcc r3, [r3], 0xfffffc99
        \\undef
        \\strcc r3, [pc, r5, ror #13]!
        \\stmdbcc sl!, {r0, r4, r5, r6, fp, ip, sp}
        \\bcc 0xfe08f0ac
        \\blcc 0xfed8f5c8
        \\stclcc 12, cr3, [r5], {66} ; 0x42
        \\stccc 13, cr3, [lr, #248]! ; 0xf8
        \\mrccc 14, 3, r3, cr1, cr4, {0}
        \\svccc 0x000e3ec5
        \\svccc 0x00843f4e
        \\svccc 0x00d33fb1
        \\svccc 0x00fb3fec
        \\svccc 0x00fb4000
        \\svccc 0x00d33fec
        \\svccc 0x00843fb1
        \\svccc 0x000e3f4e
        \\cdpcc 14, 7, cr3, cr1, cr5, {6}
        \\stccc 14, cr3, [lr, #80]! ; 0x50
        \\stclcc 13, cr3, [r5], {62} ; 0x3e
        \\blcc 0xfed8fa8c
        \\bcc 0xfe08f608
        \\stmdbcc sl!, {r1, r3, r4, r6, r7, r8, fp, ip, sp}
        \\undef
        \\ldrcc r3, [r2], -r5, ror #13
        \\ldrbcc r3, [r3], #-1334 ; 0xfffffaca
        \\rsbscc r3, r4, #-1677721599 ; 0x9c000001
        \\rsbscc r3, r6, r9, ror r1
        \\cdpcs 15, 5, cr2, cr10, cr11, {3}
        \\stccs 13, cr2, [r1], #-260 ; 0xfffffefc
        \\stmibcs sp, {r1, r3, r4, r5, r6, r7, r9, fp, sp}^
        \\undef
        \\ldrbcs r2, [sl], #1567 ; 0x61f
        \\eorscs r2, sp, #939524098 ; 0x38000002
        \\svcne 0x008b20e7
        \\stclne 14, cr1, [r6], {43} ; 0x2b
        \\stmibne pc!, {r0, r2, r3, r4, r6, r8, r9, fp, ip}^ ; <UNPREDICTABLE>
        \\undef
        \\ldrne r1, [r3], #-1423 ; 0xfffffa71
        \\undef
        \\cdpeq 15, 0, cr0, cr5, cr12, {4}
        \\beq 0xffc43bc8
        \\ldrbeq r0, [r5, r4, ror #18]
        \\ldrteq r0, [r5], #1605 ; 0x645
        \\orrseq r0, r2, r3, lsr 0x6
        \\undef
        \\undef
        \\undef
        \\undef
        \\undef
        \\mcr 0, 7, pc, cr15, cr4, {3} ; <UNPREDICTABLE>
        \\bl 0xffb7bfb0
        \\ldm r8!, {r0, r4, r5, r6, r9, fp, sp, lr, pc}^
        \\ldr lr, [r1], -r3, lsl #15
        \\teq sl, #-1560281088 ; 0xa3000000
        \\ldrsb lr, [r5], #-21 ; 0xffffffeb ; <UNPREDICTABLE>
        \\stclle 15, cr13, [r3, #100] ; 0x64
        \\blle 0x9b7be0
        \\stmiale r1!, {r0, r5, r6, r7, r8, fp, ip, lr, pc}
        \\ldrtle sp, [r3], -r7, ror #14
        \\bicsle sp, pc, #25165824 ; 0x1800000
        \\  ; <UNDEFINED> instruction: 0xd1a6d2bf
        \\svcgt 0x008ad095
        \\stcgt 14, cr12, [ip, #540] ; 0x21c
        \\blgt 0xfeb73c98
        \\stmibgt lr!, {r1, r3, r6, r7, r9, fp, lr, pc}^
        \\ldmdagt r1, {r0, r1, r3, r4, r8, fp, lr, pc}^
        \\ldrbgt ip, [r6], pc, lsl #15
        \\ldrbgt ip, [lr, #-1574]! ; 0xfffff9da
        \\strbgt ip, [sl], #-1248 ; 0xfffffb20
        \\teqgt fp, #-134217726 ; 0xf8000002
        \\subsgt ip, r2, #536870924 ; 0x2000000c
        \\orrgt ip, pc, ip, ror 0x3
        \\rscsgt ip, r2, fp, lsr r1
        \\ldrhgt ip, [ip], #-2 ; <UNPREDICTABLE>
        \\eorgt ip, sp, pc, asr #32
        \\andgt ip, r5, r4, lsl r0
        \\andgt ip, r5, r0
        \\eorgt ip, sp, r4, lsl r0
        \\rsbsgt ip, ip, pc, asr #32
        \\ldrhgt ip, [r2], #2 ; <UNPREDICTABLE>
        \\orrgt ip, pc, fp, lsr r1 ; <UNPREDICTABLE>
        \\subsgt ip, r2, #236, 2 ; 0x3b
        \\teqgt fp, #536870924 ; 0x2000000c
        \\strbgt ip, [sl], #-958 ; 0xfffffc42
        \\ldrbgt ip, [lr, #-1248]! ; 0xfffffb20
        \\ldrbgt ip, [r6], r6, lsr #12
        \\ldmdagt r1, {r0, r1, r2, r3, r7, r8, r9, sl, lr, pc}^
        \\stmibgt lr!, {r0, r1, r3, r4, r8, fp, lr, pc}^
        \\blgt 0xfeb735c0
        \\stcgt 12, cr12, [ip, #612] ; 0x264
        \\svcgt 0x008ace87
        \\  ; <UNDEFINED> instruction: 0xd1a6d095
        \\bicsle sp, pc, #-268435445 ; 0xf000000b
        \\ldrtle sp, [r3], -r6, lsl #10
        \\stmiale r1!, {r0, r1, r2, r5, r6, r8, r9, sl, ip, lr, pc}
        \\blle 0x9b7238
        \\stclle 12, cr13, [r3, #456] ; 0x1c8
        \\rsbs sp, r5, r9, lsl pc
        \\teq sl, #1073741877 ; 0x40000035
        \\ldr lr, [r1], -r3, lsr 0x9
        \\ldm r8!, {r0, r1, r7, r8, r9, sl, sp, lr, pc}^
        \\bl 0xffb7b490
        \\cdp 13, 14, cr14, cr15, cr12, {3}
        \\  ; <UNDEFINED> instruction: 0xf1fbf074
        \\  ; <UNDEFINED> instruction: 0xf50ff384
        \\  ; <UNDEFINED> instruction: 0xf82bf69c
        \\  ; <UNDEFINED> instruction: 0xfb4bf9bb
        \\  ; <UNDEFINED> instruction: 0xfe6efcdd
        \\push {r4, r5, r6, r7, r8, r9, fp, ip}
        \\ldr ip, [pc, #1432] ; 0x1084
        \\mov fp, 0x0
        \\subs r2, r2, 0x1
        \\bmi 0xb50
        \\ldrsh r4, [r0], 0x2
        \\ldrsh r6, [r0], 0x2
        \\ldrh r7, [r0], 0x2
        \\lsr r7, r7, 0x8
        \\lsl r8, r7, 0x1
        \\add r9, r7, #64 ; 0x40
        \\and r9, r9, #255 ; 0xff
        \\lsl r9, r9, 0x1
        \\ldrsh r8, [ip, r8]
        \\ldrsh r9, [ip, r9]
        \\mul r5, r4, r8
        \\sub r5, fp, r5, asr #14
        \\mul r4, r9, r4
        \\asr r4, r4, #14
        \\mul r7, r6, r9
        \\asr r7, r7, #14
        \\mul r6, r8, r6
        \\asr r6, r6, #14
        \\strh r4, [r1], r3
        \\strh r5, [r1], r3
        \\strh r6, [r1], r3
        \\strh r7, [r1], r3
        \\b 0xaec
        \\pop {r4, r5, r6, r7, r8, r9, fp, ip}
        \\bx lr
        \\push {r3, r4, r5, r6, r7, r8, r9, sl, fp, ip}
        \\ldr ip, [pc, #1312] ; 0x1084
        \\subs r2, r2, 0x1
        \\bmi 0xbf4
        \\ldm r0!, {r9, sl, fp}
        \\ldrsh r3, [r0], 0x2
        \\ldrsh r5, [r0], 0x2
        \\ldrh r6, [r0], 0x4
        \\lsr r6, r6, 0x8
        \\lsl r7, r6, 0x1
        \\add r8, r6, #64 ; 0x40
        \\and r8, r8, #255 ; 0xff
        \\lsl r8, r8, 0x1
        \\ldrsh r7, [ip, r7]
        \\ldrsh r8, [ip, r8]
        \\mul r4, r3, r7
        \\asr r4, r4, #14
        \\mul r3, r8, r3
        \\asr r3, r3, #14
        \\mul r6, r5, r8
        \\asr r6, r6, #14
        \\mul r5, r7, r5
        \\asr r5, r5, #14
        \\lsl r7, fp, #16
        \\asr r7, r7, #16
        \\rsb r7, r7, 0x0
        \\asr r8, fp, #16
        \\mla r9, r7, r3, r9
        \\mla r9, r8, r4, r9
        \\mla sl, r7, r5, sl
        \\rsb r8, r8, 0x0
        \\mla sl, r8, r6, sl
        \\strh r3, [r1], 0x2
        \\rsb r4, r4, 0x0
        \\strh r4, [r1], 0x2
        \\strh r5, [r1], 0x2
        \\strh r6, [r1], 0x2
        \\stmia r1!, {r9, sl}
        \\b 0xb60
        \\pop {r3, r4, r5, r6, r7, r8, r9, sl, fp, ip}
        \\bx lr
        \\mov fp, 0x0
        \\mov ip, #67108864 ; 0x4000000
        \\strb fp, [ip, #769] ; 0x301
        \\bx lr
        \\mov fp, #128 ; 0x80
        \\mov ip, #67108864 ; 0x4000000
        \\strb fp, [ip, #769] ; 0x301
        \\bx lr
        \\mov ip, #67108864 ; 0x4000000
        \\strb r2, [ip, #769] ; 0x301
        \\bx lr
        \\mov r0, 0x1
        \\mov r1, 0x1
        \\push {r4, lr}
        \\mov r2, 0x0
        \\mov r3, 0x1
        \\mov ip, #67108864 ; 0x4000000
        \\cmp r0, 0x0
        \\blne 0xc50
        \\add lr, pc, #28
        \\strb r2, [ip, #769] ; 0x301
        \\strb r2, [ip, #520] ; 0x208
        \\ldrh r4, [ip, #-8]
        \\ands r0, r1, r4
        \\eorne r4, r4, r0
        \\strhne r4, [ip, #-8]
        \\strb r3, [ip, #520] ; 0x208
        \\bx lr
        \\beq 0xc4c
        \\pop {r4, lr}
        \\bx lr
        \\andeq r0, r0, r0
        \\andeq r0, r0, 0x0
        \\tsteq r0, 0x0
        \\streq r0, [r0, #-0]
        \\streq r0, [r0], -r0
        \\streq r0, [r0, -r0]
        \\svcne 0x0080ffff
        \\andvs r0, r0, r0, lsl 0x2
        \\andeq r0, r0, r0, lsl 0x2
        \\nop   ; (mov r0, r0)
        \\push {r1, r2, r3, r4, fp, ip, lr}
        \\mov r3, r0
        \\ldr r0, [pc, #984] ; 0x1088
        \\tst r3, #128 ; 0x80
        \\bne 0xd68
        \\tst r3, #64 ; 0x40
        \\bne 0xd24
        \\tst r3, #32
        \\bne 0xcf8
        \\and r3, r3, #31
        \\mov r4, 0x0
        \\ldr fp, [pc, #952] ; 0x108c
        \\ldr ip, [pc, #952] ; 0x1090
        \\ldr lr, [pc, #952] ; 0x1094
        \\add r4, r4, 0x2
        \\lsrs r3, r3, 0x1
        \\beq 0xdb0
        \\bcc 0xcd8
        \\ldr r1, [fp, r4, lsl #1]
        \\ldrh r2, [ip, r4]
        \\orr r2, r2, #16777216 ; 0x1000000
        \\b 0x2c4
        \\ldr r1, [pc, #920] ; 0x1098
        \\mov r2, #32
        \\orr r2, r2, #16777216 ; 0x1000000
        \\bl 0x2c4
        \\mov r1, #67108864 ; 0x4000000
        \\add r1, r1, #256 ; 0x100
        \\mov r2, #32768 ; 0x8000
        \\strh r2, [r1, #52] ; 0x34
        \\mov r2, 0x7
        \\strh r2, [r1, #64] ; 0x40
        \\b 0xcc4
        \\ldr r1, [pc, #880] ; 0x109c
        \\mov r2, 0x8
        \\orr r2, r2, #16777216 ; 0x1000000
        \\bl 0x2c4
        \\mov r1, #67108864 ; 0x4000000
        \\mov r2, #128 ; 0x80
        \\strh r1, [r1, #128] ; 0x80
        \\strh r1, [r1, #130] ; 0x82
        \\strh r1, [r1, #132] ; 0x84
        \\strh r2, [r1, #132] ; 0x84
        \\ldrh r2, [r1, #136] ; 0x88
        \\bic r2, r2, #64512 ; 0xfc00
        \\strh r2, [r1, #136] ; 0x88
        \\mov r2, #112 ; 0x70
        \\strh r2, [r1, #112] ; 0x70
        \\strh r1, [r1, #132] ; 0x84
        \\b 0xcbc
        \\mov r1, #67108864 ; 0x4000000
        \\mov r2, #16777216 ; 0x1000000
        \\add r2, r2, #24
        \\bl 0x2c4
        \\add r1, r1, #80 ; 0x50
        \\bl 0x2c4
        \\add r1, r1, #240 ; 0xf0
        \\mov r4, 0x0
        \\str r4, [r1], 0x4
        \\str r4, [r1], 0x4
        \\str r4, [r1]
        \\sub r1, r1, #512 ; 0x200
        \\mov r4, #256 ; 0x100
        \\strh r4, [r1, #32]
        \\strh r4, [r1, #38] ; 0x26
        \\strh r4, [r1, #48] ; 0x30
        \\strh r4, [r1, #54] ; 0x36
        \\b 0xcb4
        \\pop {r1, r2, r3, r4, fp, ip, lr}
        \\bx lr
        \\nop   ; (mov r0, r0)
        \\nop   ; (mov r0, r0)
        \\mvn lr, #224, 2 ; 0x38
        \\strb lr, [r6, r4, ror #11]!
        \\bl 0xffabb570
        \\bicsle sp, r2, #208, 2 ; 0x34
        \\  ; <UNDEFINED> instruction: 0xd7d6d5d4
        \\blle 0xff6b753c
        \\bicgt ip, r2, #192, 2 ; 0x30
        \\strbgt ip, [r6, r4, asr #11]
        \\blgt 0xff2b3508
        \\  ; <UNDEFINED> instruction: 0xb3b2b1b0
        \\  ; <UNDEFINED> instruction: 0xb7b6b5b4
        \\bllt 0xfeeaf4d4
        \\  ; <UNDEFINED> instruction: 0xa3a2a1a0
        \\strge sl, [r6, r4, lsr #11]!
        \\blge 0xfeaab4a0
        \\orrsls r9, r2, #144, 2 ; 0x24
        \\  ; <UNDEFINED> instruction: 0x97969594
        \\blls 0xfe6a746c
        \\orrhi r8, r2, #128, 2
        \\strhi r8, [r6, r4, lsl #11]
        \\blhi 0xfe2a3438
        \\cmnvc r2, #112, 2
        \\  ; <UNDEFINED> instruction: 0x77767574
        \\blvc 0x1e9f404
        \\cmnvs r2, #96, 2
        \\strbvs r6, [r6, -r4, ror #10]!
        \\blvs 0x1a9b3d0
        \\cmppl r2, #80, 2
        \\  ; <UNDEFINED> instruction: 0x57565554
        \\blpl 0x169739c
        \\cmpmi r2, #64, 2
        \\strbmi r4, [r6, -r4, asr #10]
        \\blmi 0x1293368
        \\teqcc r2, #48, 2
        \\  ; <UNDEFINED> instruction: 0x37363534
        \\blcc 0xe8f334
        \\  ; <UNDEFINED> instruction: 0x23222120
        \\strcs r2, [r6, -r4, lsr #10]!
        \\blcs 0xa8b300
        \\tstne r2, #16, 2
        \\  ; <UNDEFINED> instruction: 0x17161514
        \\blne 0x6872cc
        \\tsteq r2, #0, 2
        \\streq r0, [r6, -r4, lsl #10]
        \\bleq 0x283298
        \\andhi r0, r0, r0
        \\  ; <UNDEFINED> instruction: 0x879c7c97
        \\svchi 0x00acd61e
        \\ldmdals r7!, {r1, r4, r6, ip, sp, lr, pc}
        \\cmpge r5, ip, asr #15
        \\bge 0xff702fb0
        \\strlt pc, [r4, #-820] ; 0xfffffccc
        \\svclt 0x00c886bb
        \\blgt 0xbfe344
        \\strble pc, [r4, -fp, asr #25] ; <UNPREDICTABLE>
        \\ldr pc, [r1], #-58 ; 0xffffffc6
        \\  ; <UNDEFINED> instruction: 0xf1a1bf39
        \\push {r4, r5, r6}
        \\lsl r2, r2, #24
        \\cmp r1, #178 ; 0xb2
        \\bgt 0xefc
        \\ldr r3, [pc, #484] ; 0x10a0
        \\add r4, r3, #180 ; 0xb4
        \\ldrb r5, [r3, r1]!
        \\and r6, r5, #15
        \\ldr r6, [r4, r6, lsl #2]
        \\lsr r5, r5, 0x4
        \\lsr r6, r6, r5
        \\ldrb r3, [r3, #1]
        \\and r1, r3, #15
        \\ldr r4, [r4, r1, lsl #2]
        \\lsr r3, r3, 0x4
        \\rsb r4, r6, r4, lsr r3
        \\umull r3, r1, r4, r2
        \\add r1, r1, r6
        \\ldr r2, [r0, #4]
        \\umull r3, r0, r2, r1
        \\pop {r4, r5, r6}
        \\bx lr
        \\mov r2, #-16777216 ; 0xff000000
        \\mov r1, #178 ; 0xb2
        \\b 0xeb4
        \\ldr r2, [pc, #404] ; 0x10a4
        \\mov r3, #35 ; 0x23
        \\str r2, [r0, r3]
        \\subs r3, r3, 0x1
        \\bge 0xf10
        \\bx lr
        \\andeq r0, r0, r8, lsr 0x4
        \\andeq r0, r0, r8, lsr 0x4
        \\andeq r0, r0, r8, lsr 0x4
        \\andeq r0, r0, r8, lsr 0x4
        \\andeq r0, r0, r8, lsr 0x4
        \\andeq r0, r0, r8, lsr 0x4
        \\andeq r0, r0, r8, lsr 0x4
        \\andeq r0, r0, r8, lsr 0x4
        \\andeq r0, r0, r8, lsr 0x4
        \\andeq r0, r0, r8, lsr 0x4
        \\andeq r0, r0, r8, lsr 0x4
        \\andeq r0, r0, r8, lsr 0x4
        \\andeq r0, r0, r8, lsr 0x4
        \\andeq r0, r0, r8, lsr 0x4
        \\andeq r0, r0, r8, lsr 0x4
        \\andeq r0, r0, r8, lsr 0x4
        \\andeq r0, r0, r8, lsr 0x4
        \\andeq r0, r0, r8, lsr 0x4
        \\andeq r0, r0, r8, lsr 0x4
        \\andeq r0, r0, r8, lsr 0x4
        \\andeq r0, r0, r8, lsr 0x4
        \\andeq r0, r0, r8, lsr 0x4
        \\andeq r0, r0, r8, lsr 0x4
        \\andeq r0, r0, r8, lsr 0x4
        \\andeq r0, r0, r8, lsr 0x4
        \\andeq r0, r0, r8, lsr 0x4
        \\andeq r0, r0, r8, lsr 0x4
        \\andeq r0, r0, r8, lsr 0x4
        \\andeq r0, r0, r8, lsr 0x4
        \\andeq r0, r0, r8, lsr 0x4
        \\andeq r0, r0, r8, lsr 0x4
        \\andeq r0, r0, r8, lsr 0x4
        \\andeq r0, r0, r8, lsr 0x4
        \\andeq r0, r0, r8, lsr 0x4
        \\andeq r0, r0, r8, lsr 0x4
        \\push {r4, lr}
        \\mov r4, r0
        \\mov r1, #67108864 ; 0x4000000
        \\strh r1, [r1, #198] ; 0xc6
        \\strh r1, [r1, #210] ; 0xd2
        \\mov r2, #143 ; 0x8f
        \\strh r2, [r1, #132] ; 0x84
        \\ldrh r2, [pc, #216] ; 0x10a8
        \\strh r2, [r1, #130] ; 0x82
        \\ldrb r2, [r1, #137] ; 0x89
        \\orr r2, r2, #64 ; 0x40
        \\strb r2, [r1, #137] ; 0x89
        \\add r2, r0, #848 ; 0x350
        \\str r2, [r1, #188] ; 0xbc
        \\add r2, r1, #160 ; 0xa0
        \\str r2, [r1, #192] ; 0xc0
        \\add r2, r0, #2432 ; 0x980
        \\str r2, [r1, #200] ; 0xc8
        \\add r2, r1, #164 ; 0xa4
        \\str r2, [r1, #204] ; 0xcc
        \\ldr r2, [pc, #168] ; 0x10ac
        \\str r0, [r2]
        \\mov r0, 0x0
        \\stmfd sp!, {r0}
        \\sub r0, sp, 0x4
        \\mov r1, r4
        \\mov r2, #1004 ; 0x3ec
        \\orr r2, r2, #16777216 ; 0x1000000
        \\bl 0x234
        \\ldmfd sp!, {r0}
        \\mov r0, 0x8
        \\strb r0, [r4, #6]
        \\mov r0, #15
        \\strb r0, [r4, #7]
        \\ldrh r0, [pc, #116] ; 0x10b0
        \\str r0, [r4, #56] ; 0x38
        \\ldr r0, [pc, #112] ; 0x10b4
        \\str r0, [r4, #40] ; 0x28
        \\str r0, [r4, #44] ; 0x2c
        \\str r0, [r4, #48] ; 0x30
        \\str r0, [r4, #60] ; 0x3c
        \\ldr r0, [pc, #96] ; 0x10b8
        \\str r0, [r4, #52] ; 0x34
        \\ldr r0, [pc, #92] ; 0x10bc
        \\str r0, [r4]
        \\pop {r4, lr}
        \\bx lr
        \\tsteq r0, #224, 30 ; 0x380
        \\tsteq r0, #160, 30 ; 0x280
        \\tsteq r0, #0, 30
        \\  ; <UNDEFINED> instruction: 0xfffffe00
        \\blt 0xfeb8727c
        \\  ; <UNDEFINED> instruction: 0x000007dc
        \\andeq r0, r0, r8, asr #17
        \\andeq r0, r0, r0, ror #17
        \\andeq r0, r0, r8, ror ip
        \\andeq r0, r0, r8, ror ip
        \\andeq r0, r0, lr, lsl #25
        \\  ; <UNDEFINED> instruction: 0x00000cd8
        \\streq r0, [r0], #-288 ; 0xfffffee0
        \\streq r0, [r0], #-96 ; 0xffffffa0
        \\andeq r0, r0, r0, asr #27
        \\andeq r0, r0, r0
        \\andeq sl, r0, lr, lsl #18
        \\tsteq r0, #240, 30 ; 0x3c0
        \\andeq r2, r0, r5, lsr 0x8
        \\andeq r0, r0, r8, lsr 0x4
        \\andeq r0, r0, r0, lsr #30
        \\ldmdavs r3!, {r0, r1, r4, r6, r8, sl, fp, sp, lr}^
        \\mov r1, #46592 ; 0xb600
        \\mov r0, #67108864 ; 0x4000000
        \\strh r1, [r0, #198] ; 0xc6
        \\strh r1, [r0, #210] ; 0xd2
        \\bx lr
        \\push {r4, lr}
        \\ldr r4, [pc, #176] ; 0x1190
        \\ldr r4, [r4]
        \\ldr r1, [r4]
        \\ldr r0, [pc, #168] ; 0x1194
        \\sub r0, r0, r1
        \\cmp r0, 0x1
        \\bhi 0x1138
        \\add r1, r1, 0x1
        \\str r1, [r4]
        \\mov r1, #67108864 ; 0x4000000
        \\strh r1, [r1, #198] ; 0xc6
        \\strh r1, [r1, #210] ; 0xd2
        \\strb r1, [r4, #4]
        \\mov r0, 0x0
        \\stmfd sp!, {r0}
        \\sub r0, sp, 0x4
        \\add r1, r4, #848 ; 0x350
        \\mov r2, #792 ; 0x318
        \\orr r2, r2, #16777216 ; 0x1000000
        \\bl 0x234
        \\ldmfd sp!, {r0}
        \\ldr r1, [r4]
        \\sub r1, r1, 0x1
        \\str r1, [r4]
        \\pop {r4, lr}
        \\bx lr
        \\ldr r0, [pc, #72] ; 0x1190
        \\ldr r0, [r0]
        \\ldr r1, [r0]
        \\ldr r2, [pc, #68] ; 0x1198
        \\cmp r1, r2
        \\bne 0x118c
        \\ldrb r1, [r0, #4]
        \\subs r1, r1, 0x1
        \\strb r1, [r0, #4]
        \\bgt 0x118c
        \\ldrb r1, [r0, #11]
        \\strb r1, [r0, #4]
        \\mov r0, 0x0
        \\mov r1, #46592 ; 0xb600
        \\mov r2, #67108864 ; 0x4000000
        \\strh r0, [r2, #198] ; 0xc6
        \\strh r0, [r2, #210] ; 0xd2
        \\strh r1, [r2, #198] ; 0xc6
        \\strh r1, [r2, #210] ; 0xd2
        \\bx lr
        \\tsteq r0, #240, 30 ; 0x3c0
        \\ldmdavs r3!, {r2, r4, r6, r8, sl, fp, sp, lr}^
        \\ldmdavs r3!, {r0, r1, r4, r6, r8, sl, fp, sp, lr}^
        \\nop   ; (mov r0, r0)
        \\eorseq r0, r0, r0, lsr r0
        \\ldrsheq r0, [lr], pc ; <UNPREDICTABLE>
        \\rscshi pc, r8, r1, lsl #16
        \\andhi r8, r0, #252 ; 0xfc
        \\tsthi r0, #1, 26 ; 0x40
        \\ldclvc 3, cr0, [ip], #-1008 ; 0xfffffc10
        \\addseq r0, r3, r1, lsl 0x2
        \\cdpcc 12, 3, cr7, cr12, cr2, {0}
        \\cdpcc 15, 0, cr7, cr0, cr1, {4}
        \\  ; <UNDEFINED> instruction: 0xff0100b8
        \\ldrshhi r8, [pc], #-15 ; <UNPREDICTABLE>
        \\  ; <UNDEFINED> instruction: 0xffff0100
        \\addeq r7, sp, r0, lsl #31
        \\addeq r3, r2, r0, lsl #31
        \\sbcseq r3, r3, r0, lsl #31
        \\bichi ip, r0, r1
        \\rscseq r0, pc, r0, ror 0x3
        \\tst r4, r2, lsl 0x7
        \\  ; <UNDEFINED> instruction: 0x070700ff
        \\svceq 0x00000381
        \\  ; <UNDEFINED> instruction: 0xff050085
        \\  ; <UNDEFINED> instruction: 0xf8f0f0ff
        \\ldrshhi r8, [pc, #24] ; 0x1218
        \\  ; <UNDEFINED> instruction: 0xffff09f0
        \\tsteq r1, pc, lsl #30
        \\svceq 0x000f0000
        \\  ; <UNDEFINED> instruction: 0xff020085
        \\stccc 12, cr7, [r0], {124} ; 0x7c
        \\  ; <UNDEFINED> instruction: 0xff3e3e04
        \\stcvc 8, cr7, [r0], {120} ; 0x78
        \\svceq 0x003c3c02
        \\  ; <UNDEFINED> instruction: 0xf0000093
        \\svccc 0x00000084
        \\  ; <UNDEFINED> instruction: 0xf807008d
        \\mrcvc 12, 3, APSR_nzcv, cr14, cr12, {7}
        \\ldrshhi pc, [ip], #238 ; 0xee ; <UNPREDICTABLE>
        \\  ; <UNDEFINED> instruction: 0x87e704ff
        \\stchi 7, cr0, [pc, #-28] ; 0x1220
        \\  ; <UNDEFINED> instruction: 0xf8fc0200
        \\strbeq r8, [r0], #224 ; 0xe0
        \\svccc 0x001fcfc3
        \\lfmhi f0, 3, [pc, #508]! ; 0x1448
        \\rscseq r8, pc, r0
        \\mvneq r8, #-2147483585 ; 0x8000003f
        \\svcne 0x003f7fff
        \\adcseq r0, r1, r0, lsl 0x3
        \\  ; <UNDEFINED> instruction: 0xfe80f800
        \\addeq r1, r0, r0, lsl #30
        \\svceq 0x00070304
        \\addeq ip, r0, pc, lsl #31
        \\svceq 0x001f3e04
        \\addeq r0, r5, pc, lsl #14
        \\  ; <UNDEFINED> instruction: 0xf800fe80
        \\svc 0x000a0081
        \\ldmvc fp!, {r0, r1, r2, r3, r5, r6, r7, r8, r9, sl, ip, sp, lr, pc}^
        \\mvnseq pc, #124, 28 ; 0x7c0
        \\addeq r0, r0, r3, lsl 0x2
        \\ldrbthi r3, [pc], -r1, lsl #30
        \\addgt r1, r0, r0, lsl #12
        \\  ; <UNDEFINED> instruction: 0xf8f0e0e0
        \\  ; <UNDEFINED> instruction: 0xf7ffef7c
        \\  ; <UNDEFINED> instruction: 0xe0e1f1f3
        \\  ; <UNDEFINED> instruction: 0xf9ff7fc0
        \\svcvc 0x007ffff9
        \\streq fp, [r0], #-63 ; 0xffffffc1
        \\  ; <UNDEFINED> instruction: 0xfefefcf0
        \\streq r8, [r0], #-127 ; 0xffffff81
        \\mrcvc 15, 1, r3, cr15, cr15, {0}
        \\  ; <UNDEFINED> instruction: 0x0700877e
        \\  ; <UNDEFINED> instruction: 0xfefcf080
        \\svclt 0x003f7ffe
        \\  ; <UNDEFINED> instruction: 0xfe09ff81
        \\svccc 0x003f7efe
        \\mcrvc 7, 0, r0, cr0, cr15, {0}
        \\  ; <UNDEFINED> instruction: 0x0700857f
        \\  ; <UNDEFINED> instruction: 0xff7f3f3f
        \\  ; <UNDEFINED> instruction: 0xf0fcfefe
        \\  ; <UNDEFINED> instruction: 0xff81fc80
        \\svccc 0x003fbf05
        \\rscshi pc, pc, pc, lsl pc ; <UNPREDICTABLE>
        \\streq sl, [r0], #-2175 ; 0xfffff781
        \\  ; <UNDEFINED> instruction: 0xfffffefe
        \\andeq r9, r0, #532676608 ; 0x1fc00000
        \\mrclt 15, 1, r3, cr15, cr15, {3}
        \\sbc r0, r0, r0, lsl 0x6
        \\  ; <UNDEFINED> instruction: 0x0081f8f0
        \\tsteq r7, #786432 ; 0xc0000
        \\streq r8, [r0], #-3329 ; 0xfffff2ff
        \\mrcvc 12, 7, APSR_nzcv, cr12, cr8, {7}
        \\eorseq r8, pc, lr, ror r0 ; <UNPREDICTABLE>
        \\tsteq r0, r1, lsl 0x8
        \\svcne 0x00833f3f
        \\svcne 0x00070095
        \\mrccc 15, 1, r3, cr14, cr15, {1}
        \\ldmdbls ip!, {r1, r2, r3, r4, r5, r6, sl, fp, ip, sp, lr}^
        \\  ; <UNDEFINED> instruction: 0xf0f00300
        \\addeq lr, r1, r0, ror 0x1
        \\tsteq r1, #-1073741824 ; 0xc0000000
        \\andeq r8, r0, r3, lsl #26
        \\sbchi r8, r0, r0, ror 0x9
        \\sfmhi f0, 1, [pc, #-28] ; 0x131c
        \\sbchi r8, r0, r0, lsl 0x2
        \\rscshi r0, r0, #224 ; 0xe0
        \\stchi 0, cr8, [r7, #-60] ; 0xffffffc4
        \\  ; <UNDEFINED> instruction: 0xf0f00a00
        \\mrcvc 8, 7, APSR_nzcv, cr12, cr8, {7}
        \\tsteq r3, #62, 30 ; 0xf8
        \\andhi fp, r0, r1, lsl #10
        \\svcgt 0x00c601e0
        \\streq r0, [r4, -r0, lsl #1]
        \\  ; <UNDEFINED> instruction: 0xfb630307
        \\  ; <UNDEFINED> instruction: 0xff80008d
        \\  ; <UNDEFINED> instruction: 0xf8f0c00c
        \\  ; <UNDEFINED> instruction: 0xfffd7e7e
        \\svcne 0x000703ff
        \\addeq r3, sp, pc, lsr lr
        \\teqhi r8, #256 ; 0x100
        \\rsfnee f0, f6, f0
        \\strhhi r0, [r0], r8
        \\svceq 0x00800082
        \\addhi r0, r0, sp, lsl 0x1
        \\svceq 0x0080ff82
        \\addeq pc, r0, r2, lsl #31
        \\addeq r0, r5, r2, lsl 0x7
        \\andeq r8, r1, r3, lsl 0x1
        \\strle r8, [pc], #-768 ; 0x139c
        \\  ; <UNDEFINED> instruction: 0xf8f80200
        \\andeq r8, r0, #252, 4 ; 0xc000000f
        \\stchi 3, cr0, [r3, #-4]
        \\  ; <UNDEFINED> instruction: 0xfcfc0900
        \\  ; <UNDEFINED> instruction: 0xfcf0f0f8
        \\  ; <UNDEFINED> instruction: 0x07077fff
        \\tsteq r0, r0, lsl 0x7
        \\  ; <UNDEFINED> instruction: 0xfe0100d5
        \\  ; <UNDEFINED> instruction: 0x010083fe
        \\addeq r1, sp, pc, lsl pc
        \\  ; <UNDEFINED> instruction: 0xfffefe02
        \\svceq 0x00800082
        \\mcrvc 0, 0, r0, cr5, cr4, {7}
        \\  ; <UNDEFINED> instruction: 0xfefffffe
        \\streq ip, [r0], #-126 ; 0xffffff82
        \\  ; <UNDEFINED> instruction: 0xf0e0e0c0
        \\streq r8, [r0], #-248 ; 0xffffff08
        \\tsteq r7, #3932160 ; 0x3c0000
        \\bleq 0x23bec
        \\  ; <UNDEFINED> instruction: 0xfcc0c080
        \\svccc 0x003f7efc
        \\tsteq pc, pc, lsl pc ; <UNPREDICTABLE>
        \\and r0, fp, ip, lsl 0x1
        \\  ; <UNDEFINED> instruction: 0xfcf8f8f0
        \\  ; <UNDEFINED> instruction: 0x073f7e7e
        \\strlt r0, [r1], #-259 ; 0xfffffefd
        \\  ; <UNDEFINED> instruction: 0xf0c00400
        \\ldrshhi pc, [ip], #200 ; 0xc8 ; <UNPREDICTABLE>
        \\svcvc 0x003f0100
        \\addeq pc, r3, r0, lsl #31
        \\strhi r0, [r1, #-257] ; 0xfffffeff
        \\  ; <UNDEFINED> instruction: 0xfefe0200
        \\cmneq pc, lr, ror r1 ; <UNPREDICTABLE>
        \\  ; <UNDEFINED> instruction: 0xf883fd3f
        \\andhi pc, r1, #256 ; 0x100
        \\tsteq r1, r3, lsl 0x2
        \\svcvc 0x00800085
        \\  ; <UNDEFINED> instruction: 0xfefeff0e
        \\  ; <UNDEFINED> instruction: 0xfcfcf8fc
        \\svccc 0x007ffffe
        \\tsteq r1, pc, lsl pc
        \\  ; <UNDEFINED> instruction: 0xf88100ae
        \\addeq pc, r0, r0, lsl #24
        \\orreq r0, r1, r0, lsl 0x6
        \\  ; <UNDEFINED> instruction: 0xfc82008d
        \\orreq pc, r0, r0, lsl #29
        \\  ; <UNDEFINED> instruction: 0xfe000092
        \\svcvc 0x00807e80
        \\adcseq r3, r8, r0, lsl #30
        \\  ; <UNDEFINED> instruction: 0xf0f0e004
        \\  ; <UNDEFINED> instruction: 0x0080f8f8
        \\rsbshi r3, pc, r1, lsl #30
        \\  ; <UNDEFINED> instruction: 0x000084ff
        \\tsteq r0, r1, lsl #10
        \\addeq r1, r2, r8, lsr r8
        \\  ; <UNDEFINED> instruction: 0xfc808000
        \\svcvc 0x00fefe04
        \\orreq r3, r0, pc, ror pc
        \\andgt r0, sl, sl, lsl 0x1
        \\  ; <UNDEFINED> instruction: 0xf8f0f0e0
        \\svcne 0x00fffefc
        \\  ; <UNDEFINED> instruction: 0xff820f1f
        \\tsteq r0, r0, lsl 0x1
        \\  ; <UNDEFINED> instruction: 0xf00400ac
        \\  ; <UNDEFINED> instruction: 0xfcfcf8f8
        \\svccc 0x00010080
        \\ldrbthi r8, [pc], #127 ; 0x14a8
        \\strhi r0, [r1, #-0]
        \\andseq r0, ip, r0, lsl 0x2
        \\  ; <UNDEFINED> instruction: 0xfc08f083
        \\svccc 0x00fffffc
        \\cmneq pc, pc, lsr pc ; <UNPREDICTABLE>
        \\svceq 0x0000008e
        \\  ; <UNDEFINED> instruction: 0xfe02ff81
        \\svcvc 0x00807e7f
        \\svceq 0x001f3f02
        \\strhhi r0, [r0], -r4
        \\  ; <UNDEFINED> instruction: 0xfc040080
        \\  ; <UNDEFINED> instruction: 0xfffffefc
        \\orreq r0, r1, r0, lsl 0x1
        \\andgt r0, r6, r6, lsl 0x1
        \\  ; <UNDEFINED> instruction: 0xf8f0f0e0
        \\  ; <UNDEFINED> instruction: 0xff837efc
        \\  ; <UNDEFINED> instruction: 0xff7e7e02
        \\tsteq r0, #132 ; 0x84
        \\  ; <UNDEFINED> instruction: 0xff810085
        \\  ; <UNDEFINED> instruction: 0xff810081
        \\orreq r3, r0, #516 ; 0x204
        \\adceq r0, ip, r0, lsl 0x2
        \\addeq pc, r0, r2, lsl 0x1
        \\addeq pc, r0, r2, lsl #31
        \\addeq r0, r9, r0, lsl 0x2
        \\stcle 8, cr15, [r9], {131} ; 0x83
        \\svceq 0x00010100
        \\svcvc 0x007f3f3f
        \\  ; <UNDEFINED> instruction: 0x01008fff
        \\  ; <UNDEFINED> instruction: 0xff80fe0e
        \\  ; <UNDEFINED> instruction: 0xfefefc08
        \\svccc 0x007f7fff
        \\adcseq r0, r0, pc, lsl r7
        \\sbc r8, r0, r4
        \\addeq pc, r0, r0, ror 0x1
        \\svcne 0x003f3f04
        \\addeq r0, sp, pc, lsl #30
        \\  ; <UNDEFINED> instruction: 0xfcfcf80b
        \\  ; <UNDEFINED> instruction: 0xfffffefe
        \\svccc 0x0003077f
        \\rscseq r8, pc, pc, ror r0 ; <UNPREDICTABLE>
        \\  ; <UNDEFINED> instruction: 0x010083fc
        \\addeq r0, r5, r1, lsl 0x2
        \\  ; <UNDEFINED> instruction: 0xff0e3f80
        \\  ; <UNDEFINED> instruction: 0xf8fcfeff
        \\  ; <UNDEFINED> instruction: 0xfffefcfc
        \\svceq 0x003f7fff
        \\adceq r0, lr, r1, lsl 0x2
        \\  ; <UNDEFINED> instruction: 0xff01fe80
        \\  ; <UNDEFINED> instruction: 0x800080ff
        \\svcvc 0x007f01ff
        \\tsteq r0, r0, lsl 0x1
        \\andhi r0, r3, sl, lsl 0x1
        \\sbchi ip, r0, r0, lsl 0x1
        \\svccc 0x007f04e0
        \\andshi r1, pc, pc, lsr pc ; <UNPREDICTABLE>
        \\tsteq r0, pc, lsl #26
        \\  ; <UNDEFINED> instruction: 0xf880f0f0
        \\  ; <UNDEFINED> instruction: 0xfefcfc06
        \\tsteq r3, #1835008 ; 0x1c0000
        \\adcseq r0, r1, r0, lsl 0x3
        \\  ; <UNDEFINED> instruction: 0xf0e0c004
        \\  ; <UNDEFINED> instruction: 0x0080f8f8
        \\addeq pc, r1, r2, lsl #31
        \\orreq r0, r0, #0, 2
        \\  ; <UNDEFINED> instruction: 0xf8810085
        \\  ; <UNDEFINED> instruction: 0xfefcf004
        \\  ; <UNDEFINED> instruction: 0xff80f1fe
        \\  ; <UNDEFINED> instruction: 0xffff7f06
        \\  ; <UNDEFINED> instruction: 0x010303fc
        \\tsteq r1, r0, lsl 0x1
        \\andhi r8, r0, r1, lsl #10
        \\  ; <UNDEFINED> instruction: 0xfeff077f
        \\  ; <UNDEFINED> instruction: 0xf8f0fcfe
        \\  ; <UNDEFINED> instruction: 0xff80fcf8
        \\andshi r7, pc, #1, 30
        \\streq sl, [r0], #-2817 ; 0xfffff4ff
        \\  ; <UNDEFINED> instruction: 0xfefefcf0
        \\streq r8, [r0], #-255 ; 0xffffff01
        \\  ; <UNDEFINED> instruction: 0xff7f7f1f
        \\tsteq r0, #16320 ; 0x3fc0
        \\svccc 0x003f7f7f
        \\  ; <UNDEFINED> instruction: 0xfe00ff80
        \\  ; <UNDEFINED> instruction: 0xff80fc81
        \\orreq r7, r1, r0, lsl #30
        \\  ; <UNDEFINED> instruction: 0xfc0f0089
        \\  ; <UNDEFINED> instruction: 0xf0e0e0c0
        \\svcvc 0x00fcf8f8
        \\svceq 0x000f1f3f
        \\stcgt 3, cr0, [r1, #-28] ; 0xffffffe4
        \\mvnshi r0, r0
        \\ldrshhi r0, [r8], #12 ; <UNPREDICTABLE>
        \\andls r8, r1, #0, 2
        \\  ; <UNDEFINED> instruction: 0xfe7e0500
        \\mrcvc 15, 7, APSR_nzcv, cr14, cr15, {7}
        \\strh r0, [r0], -r8
        \\addeq pc, r0, r1, lsl 0x1
        \\svceq 0x00010780
        \\stmdbeq r0, {r0, r1, r2, r8, sl, fp, pc}
        \\andeq ip, r0, r0, ror 0x1
        \\  ; <UNDEFINED> instruction: 0xfcfcf8f0
        \\addeq r0, r0, r7, lsl 0x6
        \\tsteq r3, #-2147483648 ; 0x80000000
        \\  ; <UNDEFINED> instruction: 0xfc09008d
        \\  ; <UNDEFINED> instruction: 0xf0f0f8fc
        \\  ; <UNDEFINED> instruction: 0x071f7ffe
        \\andeq r8, r3, r7
        \\andeq fp, r0, #256 ; 0x100
        \\mvnshi pc, r0, asr #17
        \\strhi r8, [r3, #-256] ; 0xffffff00
        \\  ; <UNDEFINED> instruction: 0xf0c00200
        \\  ; <UNDEFINED> instruction: 0x07ff84fe
        \\andeq r0, r1, pc, lsr pc
        \\tsteq r3, r3, lsl #30
        \\  ; <UNDEFINED> instruction: 0xfc02008b
        \\addeq r8, r2, r0, ror 0x1
        \\  ; <UNDEFINED> instruction: 0xffff7f05
        \\  ; <UNDEFINED> instruction: 0x8080f0fc
        \\andeq r8, r3, r0, lsl 0x4
        \\andeq sl, r0, r2, lsl #26
        \\  ; <UNDEFINED> instruction: 0x000084ff
        \\  ; <UNDEFINED> instruction: 0x000084ff
        \\tsthi r0, r3, lsl #10
        \\  ; <UNDEFINED> instruction: 0x000001ff
        \\andeq pc, r3, r3, lsl #31
        \\mvnshi pc, r0, lsl #30
        \\andeq r0, r0, r3, lsl 0x6
        \\addeq r0, r5, r3, lsl 0x6
        \\addeq pc, r2, r0, lsl #31
        \\addeq pc, r2, r0, lsl #31
        \\adceq r0, lr, r0, lsl 0x7
        \\svcvc 0x001f0303
        \\  ; <UNDEFINED> instruction: 0x000084ff
        \\tsteq r0, #3, 26 ; 0xc0
        \\ldrshgt pc, [r0], #239 ; 0xef ; <UNPREDICTABLE>
        \\and r0, fp, r0, lsl 0x1
        \\  ; <UNDEFINED> instruction: 0xffff3f0f
        \\  ; <UNDEFINED> instruction: 0xfffffcfe
        \\andhi r0, r1, 0x0
        \\andeq r8, r0, r3, lsl #10
        \\  ; <UNDEFINED> instruction: 0x07ff80f8
        \\andeq r0, r1, pc, lsr pc
        \\  ; <UNDEFINED> instruction: 0x01071fff
        \\  ; <UNDEFINED> instruction: 0xfc0000b4
        \\  ; <UNDEFINED> instruction: 0xff00fe80
        \\streq r0, [r4, -r0, lsl #1]
        \\svcne 0x001f0f0f
        \\strgt r0, [sp, -sp, lsl #1]
        \\  ; <UNDEFINED> instruction: 0xf8f0e0c3
        \\svcne 0x007cfcfc
        \\  ; <UNDEFINED> instruction: 0x070f0f1f
        \\andeq r8, r0, #3, 30
        \\teqhi lr, lr, ror lr
        \\bls 0xf8190c
        \\addgt r0, r0, r0, lsl 0x4
        \\tsteq r0, #240, 2 ; 0x3c
        \\svccc 0x00fffffe
        \\svccc 0x00030081
        \\mvnshi pc, #1020 ; 0x3fc
        \\streq r1, [r3, -r0, lsl #6]
        \\mrccc 8, 3, APSR_nzcv, cr12, cr8, {7}
        \\svcgt 0x00cf9e9e
        \\  ; <UNDEFINED> instruction: 0xfffefc03
        \\  ; <UNDEFINED> instruction: 0x070f1fbf
        \\svcvc 0x0081ffe0
        \\svceq 0x003e3e04
        \\cdpne 15, 8, cr1, cr0, cr15, {0}
        \\ldrthi r1, [ip], #-3073 ; 0xfffff3ff
        \\bicshi r0, pc, pc, asr 0x1
        \\strhi r3, [r7, r7, lsl #4]
        \\svccc 0x003effff
        \\svcle 0x009f1f1f
        \\mrcne 15, 0, APSR_nzcv, cr12, cr15, {7}
        \\svceq 0x000f1f1e
        \\cdpcc 3, 11, cr0, cr14, cr7, {0}
        \\ldrsh pc, [r0], #140 ; 0x8c ; <UNPREDICTABLE>
        \\svcvc 0x00ff0080
        \\  ; <UNDEFINED> instruction: 0xff3f0300
        \\svcvc 0x00fffeff
        \\  ; <UNDEFINED> instruction: 0xfffcc000
        \\stccc 15, cr7, [r1], {255} ; 0xff
        \\  ; <UNDEFINED> instruction: 0x070f1f3f
        \\streq sl, [r0], #-2307 ; 0xfffff6fd
        \\  ; <UNDEFINED> instruction: 0xfefefcfc
        \\andhi r8, r0, #255 ; 0xff
        \\streq r8, [r0], -r3, lsl #12
        \\sbcgt r8, r0, r0, lsl 0x1
        \\rscshi pc, r0, r0, ror 0x1
        \\svc 0x00ef04ff
        \\sbchi ip, r3, #60555264 ; 0x39c0000
        \\strhi r8, [pc, #-7] ; 0x179d
        \\  ; <UNDEFINED> instruction: 0xf8f80700
        \\  ; <UNDEFINED> instruction: 0xfefefcfc
        \\  ; <UNDEFINED> instruction: 0xff807f7f
        \\andhi ip, r3, r0, lsl 0x1
        \\tsthi pc, #128, 30 ; 0x200
        \\andhi sl, r0, #2031616 ; 0x1f0000
        \\  ; <UNDEFINED> instruction: 0x800080f8
        \\ldrsh r0, [sp, #31]!
        \\tsteq r4, r0, lsl 0x1
        \\svceq 0x000f0707
        \\  ; <UNDEFINED> instruction: 0xfc840085
        \\mvn pc, r3, lsl #28
        \\ldmibeq pc!, {r0, r2, r3, r4, r5, r6, r7, pc}^ ; <UNPREDICTABLE>
        \\svceq 0x000fe0fc
        \\streq r0, [r3, -r7, lsl #6]
        \\addeq r0, r5, pc, lsl #30
        \\cdpvc 14, 0, cr15, cr1, cr0, {4}
        \\rscshi r8, pc, lr, ror r0 ; <UNPREDICTABLE>
        \\  ; <UNDEFINED> instruction: 0xfce001c0
        \\svcne 0x0080ff80
        \\streq r0, [r1, -r0, lsl #31]
        \\tsteq r0, #16384 ; 0x4000
        \\  ; <UNDEFINED> instruction: 0xf8f8f0c0
        \\  ; <UNDEFINED> instruction: 0xff820080
        \\svcne 0x00810080
        \\addeq r0, r5, r0, lsl #30
        \\  ; <UNDEFINED> instruction: 0xfe80fc00
        \\svceq 0x00027f81
        \\addeq r0, r2, r3, lsl 0x2
        \\addeq r0, ip, r0, lsl #24
        \\  ; <UNDEFINED> instruction: 0xfe80ff00
        \\  ; <UNDEFINED> instruction: 0xf0f8fc06
        \\tsthi r0, #192 ; 0xc0
        \\stmdaeq r1, {r1, r7, r8, r9, sl, fp, ip, sp, lr, pc}
        \\andeq r8, pc, #14
        \\stmdage r1, {r0, r1, r2, r8, r9, sl}
        \\rscshi r8, r8, r0, lsl 0x4
        \\rscshi r8, pc, r0, lsl 0x4
        \\streq r0, [r1, -r0, lsl #8]
        \\ldrhi r1, [pc, #-3855]! ; 0x935
        \\ldrbeq r8, [ip, #768]! ; 0x300
        \\  ; <UNDEFINED> instruction: 0x81e1fefe
        \\addeq r0, r1, r1, lsl 0x2
        \\svcvc 0x00803f00
        \\svcvc 0x007e7e03
        \\streq r8, [r0], #-1407 ; 0xfffffa81
        \\  ; <UNDEFINED> instruction: 0xfe7efefe
        \\rscseq r8, pc, #254 ; 0xfe
        \\rschi r8, r0, 0x0
        \\svcvc 0x007f07ff
        \\svceq 0x001f3f3f
        \\adceq r0, r8, r7, lsl 0x2
        \\addeq pc, r0, r2, lsl #17
        \\addeq pc, r0, r2, lsl #31
        \\orreq r0, r1, #0, 14
        \\  ; <UNDEFINED> instruction: 0xfc840085
        \\tstpeq r1, r2, lsl #28
        \\addeq pc, lr, r2, lsl #31
        \\cdpvc 14, 15, cr15, cr14, cr4, {0}
        \\  ; <UNDEFINED> instruction: 0xff80fefe
        \\  ; <UNDEFINED> instruction: 0xff820080
        \\orreq r0, r1, #128 ; 0x80
        \\adceq r0, r8, r0, lsl 0x2
        \\addeq pc, r0, r2, lsl #17
        \\addeq pc, r0, r2, lsl #31
        \\stmdahi r1, {r0, r8}
        \\rscseq r8, ip, #0, 8
        \\andhi r0, r1, #-2147483585 ; 0x8000003f
        \\andhi r8, r0, pc, lsr lr
        \\mrcvc 1, 3, r0, cr14, cr14, {7}
        \\adcseq r7, r9, r0, lsl #31
        \\  ; <UNDEFINED> instruction: 0xf8f0c003
        \\andhi r8, r0, #252 ; 0xfc
        \\  ; <UNDEFINED> instruction: 0x810080ff
        \\ldrhi r0, [pc, #-127]! ; 0x1851
        \\rscshi r0, ip, r0
        \\rsbseq r8, pc, #-2147483585 ; 0x8000003f
        \\andhi r0, r1, #1006632960 ; 0x3c000000
        \\  ; <UNDEFINED> instruction: 0x003002f0
        \\teqeq pc, r0
        \\addeq r1, r5, pc, lsl pc
        \\  ; <UNDEFINED> instruction: 0xfe80ff00
        \\  ; <UNDEFINED> instruction: 0xf0f8fc06
        \\orrhi r8, r0, #192 ; 0xc0
        \\svcne 0x0082ff82
        \\tsteq pc, #2, 30
        \\  ; <UNDEFINED> instruction: 0xf88200a8
        \\orreq r0, r0, #128 ; 0x80
        \\andhi r0, r1, r1, lsl 0x2
        \\rscseq r8, r8, r0, lsl 0x2
        \\  ; <UNDEFINED> instruction: 0x000080fc
        \\strhi r8, [r1], #-259 ; 0xfffffefd
        \\ldrshhi r0, [lr], #12 ; <UNPREDICTABLE>
        \\rscshi r8, pc, r1, lsl 0x4
        \\ldrshhi r8, [pc], #44 ; <UNPREDICTABLE>
        \\andhi r8, r0, r1, lsl 0x4
        \\mrcvc 1, 3, r0, cr14, cr14, {7}
        \\addeq r7, r5, r0, lsl #31
        \\cdpvc 14, 8, cr15, cr0, cr0, {0}
        \\adceq r7, r8, r1, lsl #31
        \\addeq pc, r0, r2, lsl #17
        \\tsteq r1, r0, lsl 0x7
        \\strhi r8, [r0], #-3329 ; 0xfffff2ff
        \\ldrshhi r0, [lr, #12]!
        \\andhi r9, r0, r1, lsl 0x2
        \\mrcvc 1, 3, r0, cr14, cr14, {7}
        \\addseq r7, r9, r0, lsl #31
        \\andgt r8, r0, r0, lsl 0x1
        \\svccc 0x00000081
        \\addeq r1, sp, r0, lsl #31
        \\and ip, r1, r3, lsl 0x1
        \\andshi r8, pc, #224 ; 0xe0
        \\tsthi r0, pc, lsl #26
        \\mvnseq r8, r0, ror 0x3
        \\streq r0, [r3, pc, lsl #30]
        \\  ; <UNDEFINED> instruction: 0xf002008d
        \\  ; <UNDEFINED> instruction: 0xff81f8f0
        \\orreq r7, r1, #0, 30
        \\andlt r0, r1, #1073741824 ; 0x40000000
        \\rscshi r8, r8, r0, lsl 0x4
        \\orrgt r0, r3, #0, 8
        \\rscshi lr, r1, r3, asr 0x3
        \\svcvc 0x00ff0400
        \\strhi r1, [pc, #-3903] ; 0xa55
        \\bleq 0xfff22998
        \\  ; <UNDEFINED> instruction: 0xfffdf9fe
        \\  ; <UNDEFINED> instruction: 0xffff7fff
        \\  ; <UNDEFINED> instruction: 0x010307fe
        \\tsteq r0, r1, lsl 0x1
        \\  ; <UNDEFINED> instruction: 0xfe800085
        \\rsbshi r7, lr, r1, lsl #28
        \\  ; <UNDEFINED> instruction: 0xfcfc0f7f
        \\ldrsh pc, [r0], #8 ; <UNPREDICTABLE>
        \\tsteq r3, #192 ; 0xc0
        \\svcne 0x000f0f07
        \\adceq r3, r8, pc, lsr pc
        \\addeq pc, r0, r2, lsl #17
        \\andhi r0, r3, r1, lsl 0x6
        \\strhi r8, [r0], #-3329 ; 0xfffff2ff
        \\ldrshhi r0, [lr, #12]!
        \\streq r9, [r0], #-257 ; 0xfffffeff
        \\  ; <UNDEFINED> instruction: 0xfe7efefe
        \\ldrshhi r8, [pc], #14 ; <UNPREDICTABLE>
        \\rscshi r8, pc, r0, lsl 0x4
        \\stmdage r1, {r9, pc}
        \\addhi r0, r0, r0
        \\rschi r0, r0, r0, asr 0x1
        \\svccc 0x003f0100
        \\addeq r7, r0, r0, lsl #31
        \\addgt r8, r0, r4
        \\addeq lr, r0, r0, ror 0x1
        \\svcvc 0x00023f81
        \\  ; <UNDEFINED> instruction: 0xf080e0e0
        \\svcvc 0x0000f880
        \\  ; <UNDEFINED> instruction: 0xfb08ff81
        \\  ; <UNDEFINED> instruction: 0xf0f0f9fb
        \\  ; <UNDEFINED> instruction: 0xfdfcf8f8
        \\cdpvc 15, 8, cr7, cr0, cr4, {4}
        \\  ; <UNDEFINED> instruction: 0xfe08fc80
        \\svcvc 0x007f7efe
        \\  ; <UNDEFINED> instruction: 0xf0f0f1f1
        \\svccc 0x0007e081
        \\svceq 0x001f1f3f
        \\strhi r0, [r3, #-1799] ; 0xfffff8f9
        \\andhi sl, r0, #126 ; 0x7e
        \\streq r8, [r0], #-248 ; 0xffffff08
        \\svceq 0x00070703
        \\andhi r8, r0, pc, lsl r0
        \\  ; <UNDEFINED> instruction: 0xfcfc01f8
        \\orreq r0, r2, r0, lsl 0x1
        \\  ; <UNDEFINED> instruction: 0xfe07fc84
        \\svcvc 0x007f3f1f
        \\rscshi pc, lr, #4080 ; 0xff0
        \\ldrshhi r0, [lr], #12 ; <UNPREDICTABLE>
        \\  ; <UNDEFINED> instruction: 0x010101ff
        \\  ; <UNDEFINED> instruction: 0xfe800083
        \\rsbshi r7, lr, r1, lsl #28
        \\  ; <UNDEFINED> instruction: 0xf8f8077f
        \\  ; <UNDEFINED> instruction: 0xc0e0e0f0
        \\svcvc 0x00848080
        \\adceq r3, r9, r0, lsl #30
        \\  ; <UNDEFINED> instruction: 0xf8f0c003
        \\andhi r8, r0, #252 ; 0xfc
        \\streq r8, [r0], #-255 ; 0xffffff01
        \\svcvc 0x003f1f07
        \\andeq r8, r0, pc, ror r5
        \\ldrshhi r8, [lr, #12]!
        \\orreq r0, r7, pc, ror r2
        \\andeq r8, r0, #268435456 ; 0x10000000
        \\ldrshhi pc, [lr], #239 ; 0xef ; <UNPREDICTABLE>
        \\  ; <UNDEFINED> instruction: 0xfefe01fc
        \\tsteq r1, r0, lsl 0x1
        \\andeq r8, r0, r1
        \\  ; <UNDEFINED> instruction: 0x06fe80ff
        \\ldrshgt pc, [r0], #140 ; 0x8c ; <UNPREDICTABLE>
        \\strbhi r0, [r3], 0x0
        \\svcvc 0x007f05ff
        \\tsteq r7, pc, lsr pc
        \\  ; <UNDEFINED> instruction: 0xf88200a8
        \\  ; <UNDEFINED> instruction: 0xff810080
        \\addeq pc, r0, r0, lsl 0x2
        \\streq r0, [r3, -r4, lsl #2]
        \\addeq r0, r5, pc, lsl #30
        \\  ; <UNDEFINED> instruction: 0xfe00fc84
        \\  ; <UNDEFINED> instruction: 0xf101e180
        \\ldrshhi r8, [pc, #14] ; 0x1aea
        \\streq r0, [r7, -pc, lsl #6]
        \\addeq r0, r5, r3, lsl 0x2
        \\cdpvc 14, 0, cr15, cr1, cr0, {4}
        \\ldmdbls pc!, {r1, r2, r3, r4, r5, r6, pc}^ ; <UNPREDICTABLE>
        \\  ; <UNDEFINED> instruction: 0xf0c00300
        \\  ; <UNDEFINED> instruction: 0x0080f8f8
        \\addeq pc, r0, r2, lsl #31
        \\svccc 0x001f0704
        \\addeq r7, r5, pc, ror pc
        \\  ; <UNDEFINED> instruction: 0xfe80fc00
        \\strhi r7, [r2, -r1, lsl #31]
        \\addeq r0, r2, r1, lsl 0x2
        \\  ; <UNDEFINED> instruction: 0xfefeff02
        \\  ; <UNDEFINED> instruction: 0xfe01fc80
        \\streq r8, [r0, #-510] ; 0xfffffe02
        \\andeq r0, r0, r1, lsl 0x2
        \\  ; <UNDEFINED> instruction: 0xfe80ff7f
        \\rscs pc, r8, r5, lsl #24
        \\addhi r0, r1, 0x0
        \\  ; <UNDEFINED> instruction: 0xfffe07ff
        \\svcne 0x003f7f7f
        \\addeq r0, r5, pc, lsl #14
        \\addhi ip, r0, r2
        \\  ; <UNDEFINED> instruction: 0xff810082
        \\rscs pc, r8, fp, lsl #24
        \\streq r0, [r1, -r0, lsl #1]
        \\  ; <UNDEFINED> instruction: 0x070f1f3f
        \\andhi sl, r0, #196608 ; 0x30000
        \\  ; <UNDEFINED> instruction: 0x800080f8
        \\ldrsh r0, [sp, #31]!
        \\tsteq r1, #128 ; 0x80
        \\strhi r8, [pc, #-7] ; 0x1b55
        \\ldrbteq r8, [ip], #1024 ; 0x400
        \\  ; <UNDEFINED> instruction: 0xe1c1c1fe
        \\ldrshhi r8, [pc], #29 ; <UNPREDICTABLE>
        \\svceq 0x000f041f
        \\strhi r0, [r1, #-263] ; 0xfffffef9
        \\  ; <UNDEFINED> instruction: 0xfefe0100
        \\svcvc 0x00807e80
        \\  ; <UNDEFINED> instruction: 0xf8f8fc0f
        \\  ; <UNDEFINED> instruction: 0xe0e0f0f0
        \\streq r0, [r3, -r0, asr #7]
        \\svcne 0x000f0f07
        \\streq sl, [r0], #-2079 ; 0xfffff7e1
        \\  ; <UNDEFINED> instruction: 0xfcf8f0e0
        \\  ; <UNDEFINED> instruction: 0x000080fc
        \\mvnshi r8, pc, ror r1
        \\mrseq r0, (UNDEF: 17)
        \\  ; <UNDEFINED> instruction: 0xfc800087
        \\rscs pc, r0, ip, lsl #16
        \\mvneq r8, r0, asr 0x1
        \\svcne 0x000f0703
        \\addeq r7, lr, pc, lsr pc
        \\tsthi lr, r1, lsl 0x4
        \\svcvc 0x00fc02ff
        \\cmneq pc, lr, ror r1 ; <UNPREDICTABLE>
        \\adcseq r0, r0, pc, lsr pc
        \\rscshi pc, lr, r1, lsl #28
        \\andhi r8, r0, #255 ; 0xff
        \\andhi r8, r0, #255 ; 0xff
        \\andhi r8, r0, pc, lsl r5
        \\sbceq r8, r0, r0, lsl 0x5
        \\andseq r8, pc, pc, lsr r3 ; <UNPREDICTABLE>
        \\tsteq r0, pc, lsl #26
        \\add ip, r3, r0, asr 0x1
        \\streq r0, [r1, -r3, lsl #31]
        \\tsthi r0, r7
        \\ldrshhi r0, [lr], #12 ; <UNPREDICTABLE>
        \\mrseq r0, (UNDEF: 17)
        \\  ; <UNDEFINED> instruction: 0xfc800083
        \\ldrbhi pc, [lr, #3585]! ; 0xe01 ; <UNPREDICTABLE>
        \\rscshi r8, lr, r0
        \\svcvc 0x007f017e
        \\  ; <UNDEFINED> instruction: 0xfe030085
        \\ldrshhi r7, [lr, #-238]! ; 0xffffff12
        \\tsteq r0, pc, ror r5
        \\  ; <UNDEFINED> instruction: 0xfe807f7f
        \\rscs pc, r8, r5, lsl #24
        \\sbchi r8, r1, 0x0
        \\ldrteq r8, [pc], #-255 ; 0x1c1c
        \\  ; <UNDEFINED> instruction: 0x070f1f1f
        \\streq sl, [r0], #-2049 ; 0xfffff7ff
        \\mrcvc 15, 3, r3, cr15, cr15, {1}
        \\streq r8, [r0], #-126 ; 0xffffff82
        \\  ; <UNDEFINED> instruction: 0xf8f0f0e0
        \\streq r8, [r0], #-248 ; 0xffffff08
        \\tsteq r7, #3932160 ; 0x3c0000
        \\andeq r8, r0, #12582912 ; 0xc00000
        \\ldrbthi r7, [lr], #3710 ; 0xe7e
        \\  ; <UNDEFINED> instruction: 0xfefe07fc
        \\svccc 0x003f7f7f
        \\addeq r0, fp, r1, lsl 0x2
        \\  ; <UNDEFINED> instruction: 0xf081f881
        \\svceq 0x000f1f07
        \\tsteq r3, #1835008 ; 0x1c0000
        \\andeq fp, r0, r1
        \\rsbshi r8, lr, pc, ror r1
        \\rsc r0, r0, r0, lsl 0x8
        \\ldrshhi pc, [r8], #0 ; <UNPREDICTABLE>
        \\andeq r8, r7, r0, lsl 0x2
        \\streq r8, [r0, #-15]
        \\svcvc 0x00fefefc
        \\  ; <UNDEFINED> instruction: 0xfc81fe7f
        \\  ; <UNDEFINED> instruction: 0xfc0af881
        \\  ; <UNDEFINED> instruction: 0xfffefefc
        \\svchi 0x008f3fbf
        \\  ; <UNDEFINED> instruction: 0xff81dfcf
        \\svcne 0x003f3f09
        \\  ; <UNDEFINED> instruction: 0x070f0f1f
        \\mvnshi pc, r7, lsl #16
        \\  ; <UNDEFINED> instruction: 0xe0e009f0
        \\svceq 0x000f1f1f
        \\tsteq r3, r7, lsl #14
        \\  ; <UNDEFINED> instruction: 0xfe08ff80
        \\ldclcc 14, cr7, [ip], #-1016 ; 0xfffffc08
        \\tsteq r1, r3, lsl 0x6
        \\  ; <UNDEFINED> instruction: 0xf80400a4
        \\  ; <UNDEFINED> instruction: 0xc0e0e0f0
        \\streq r0, [r4, -r0, lsl #1]
        \\svclt 0x001f0f0f
        \\  ; <UNDEFINED> instruction: 0xfc040080
        \\svcvc 0x00fffefe
        \\streq r0, [r6, -r0, lsl #1]
        \\andeq r0, r0, r3, lsl 0x2
        \\addeq r8, r2, r0, lsl 0x1
        \\  ; <UNDEFINED> instruction: 0xff818000
        \\  ; <UNDEFINED> instruction: 0xfffefe0b
        \\svceq 0x001f3fff
        \\tsteq r3, #3932160 ; 0x3c0000
        \\streq r8, [r0], -r7, lsl #10
        \\  ; <UNDEFINED> instruction: 0xf0e0c080
        \\ldrshhi pc, [lr, #200]! ; 0xc8 ; <UNPREDICTABLE>
        \\  ; <UNDEFINED> instruction: 0xc7ef0cff
        \\streq r8, [r1, -r3, lsl #7]
        \\svccc 0x001f0f0f
        \\ldmge pc!, {r0, r1, r2, r3, r4, r5, r8, r9, sl, fp, ip, sp, lr}^ ; <UNPREDICTABLE>
        \\svccc 0x003f0400
        \\rsbshi r7, lr, pc, ror lr
        \\  ; <UNDEFINED> instruction: 0xf8f00400
        \\ldrshhi pc, [lr], #200 ; 0xc8 ; <UNPREDICTABLE>
        \\streq r0, [pc, -r0, lsl #8]
        \\strhi r0, [r1, #-259] ; 0xfffffefd
        \\rscshi r0, lr, r0
        \\  ; <UNDEFINED> instruction: 0xf8f80bfc
        \\svcvc 0x00fef0f0
        \\svcne 0x001f3f7f
        \\addeq r0, sp, pc, lsl #14
        \\  ; <UNDEFINED> instruction: 0xf881f081
        \\tsthi r7, #262144 ; 0x40000
        \\andhi fp, r0, r3
        \\  ; <UNDEFINED> instruction: 0xf0f001e0
        \\  ; <UNDEFINED> instruction: 0xff820080
        \\  ; <UNDEFINED> instruction: 0xff800080
        \\eorshi r7, pc, r1, lsl #30
        \\mrseq r0, (UNDEF: 19)
        \\andhi r0, lr, r7, lsl 0x1
        \\  ; <UNDEFINED> instruction: 0xf8f0e0c0
        \\svcvc 0x00fffefc
        \\  ; <UNDEFINED> instruction: 0x070f0f1f
        \\addeq r0, r7, r3, lsl 0x2
        \\sbc r8, r0, sl
        \\  ; <UNDEFINED> instruction: 0xfefcf8f0
        \\svceq 0x001f3fff
        \\addeq pc, r0, r2, lsl #31
        \\addeq r3, r9, r2, lsl #31
        \\addeq pc, r1, r1, lsl 0x1
        \\addeq r3, sp, r1, lsl #31
        \\  ; <UNDEFINED> instruction: 0xf883f000
        \\teqphi pc, #256 ; 0x100
        \\andhi r8, r0, #1, 28
        \\mrcvc 2, 3, r0, cr12, cr12, {7}
        \\andhi r9, r0, lr, ror r5
        \\  ; <UNDEFINED> instruction: 0xfefe017e
        \\addeq pc, r0, r0, lsl #31
        \\adcseq r0, r0, r2, lsl 0x7
        \\cdpcc 15, 3, cr1, cr15, cr4, {0}
        \\addseq r7, r5, lr, ror ip
        \\  ; <UNDEFINED> instruction: 0xf8fc7c07
        \\ldrsh pc, [r0], #8 ; <UNPREDICTABLE>
        \\andhi r8, r0, r0, ror 0x1
        \\tsteq r3, #1073741824 ; 0x40000000
        \\and r0, r2, sp, lsl 0x1
        \\addhi ip, r0, r0, asr 0x1
        \\andhi r0, r0, r1
        \\svceq 0x000f0407
        \\teqls pc, pc, lsl pc ; <UNPREDICTABLE>
        \\mvnshi r8, r0, lsl 0x2
        \\ldfhid f0, [pc, #-0] ; 0x1dcc
        \\rscseq r0, r0, r0, lsl 0x4
        \\orreq r8, r0, r0, lsl 0x4
        \\svcne 0x00833f3f
        \\andhi r0, r1, sp, lsl 0x1
        \\sbceq r8, r0, r0, lsl 0x7
        \\andeq r8, pc, pc, lsl r3 ; <UNPREDICTABLE>
        \\andhi r8, r0, r7, lsl #26
        \\mvnseq r8, r0, ror 0x1
        \\  ; <UNDEFINED> instruction: 0x0783ffff
        \\svcge 0x00030301
        \\sbcgt r0, r0, r0, lsl #26
        \\  ; <UNDEFINED> instruction: 0xf0f0e0e0
        \\svcne 0x000f0000
        \\svcvc 0x003f3f1f
        \\  ; <UNDEFINED> instruction: 0xf80b008d
        \\  ; <UNDEFINED> instruction: 0xfefcfcf8
        \\svcvc 0x00007f7e
        \\ldrshhi pc, [ip], #223 ; 0xdf ; <UNPREDICTABLE>
        \\tsteq r0, #248, 2 ; 0x3e
        \\tsteq r3, #1073741824 ; 0x40000000
        \\  ; <UNDEFINED> instruction: 0x008a00ff
        \\mvnshi pc, #1, 30
        \\  ; <UNDEFINED> instruction: 0xffff0100
        \\tsteq r1, #131 ; 0x83
        \\streq sl, [r0], #-1539 ; 0xfffff9fd
        \\ldclvc 14, cr3, [ip], #-252 ; 0xffffff04
        \\strls pc, [r0, #-4088] ; 0xfffff008
        \\  ; <UNDEFINED> instruction: 0xf8c00700
        \\  ; <UNDEFINED> instruction: 0xfffefefc
        \\  ; <UNDEFINED> instruction: 0xff823f7f
        \\  ; <UNDEFINED> instruction: 0xfcfcf802
        \\addeq r0, r6, r4, lsl 0x3
        \\  ; <UNDEFINED> instruction: 0xff033f80
        \\ldrshhi pc, [lr], #239 ; 0xef ; <UNPREDICTABLE>
        \\ldrshhi r0, [lr], #12 ; <UNPREDICTABLE>
        \\svcvc 0x007f01ff
        \\  ; <UNDEFINED> instruction: 0xf88200b0
        \\tsteq r1, #128 ; 0x80
        \\stchi 0, cr8, [r1, #-12]
        \\  ; <UNDEFINED> instruction: 0xf8f80100
        \\  ; <UNDEFINED> instruction: 0xff82fc83
        \\  ; <UNDEFINED> instruction: 0xf0f0f104
        \\orreq r0, r0, #0, 2
        \\addeq r0, r5, r0, lsl #15
        \\cdpvc 14, 15, cr15, cr14, cr2, {0}
        \\  ; <UNDEFINED> instruction: 0xf002fe82
        \\  ; <UNDEFINED> instruction: 0xff80f8f0
        \\ldreq r7, [pc, -r7, lsl #30]!
        \\tsteq r3, r7, lsl 0x6
        \\stmdaeq r0, {r0, r8, r9, sl, lr, pc}
        \\  ; <UNDEFINED> instruction: 0xfefcf8e0
        \\svccc 0x007ffffe
        \\ldrsheq r8, [pc], #-31 ; <UNPREDICTABLE>
        \\bleq 0x25c24
        \\  ; <UNDEFINED> instruction: 0xff7f3f3f
        \\  ; <UNDEFINED> instruction: 0xf0fcfefe
        \\svcvc 0x00700000
        \\adcseq r3, r8, r1, lsl #31
        \\addeq pc, r0, r2, lsl 0x1
        \\andhi r0, r7, r1, lsl #14
        \\streq r8, [r0, -r3, lsl #10]
        \\  ; <UNDEFINED> instruction: 0xfefcf8e0
        \\svcvc 0x007ffffe
        \\  ; <UNDEFINED> instruction: 0xf902ff82
        \\orreq pc, r0, #248, 16 ; 0xf80000
        \\addeq r0, r5, r2, lsl 0x3
        \\  ; <UNDEFINED> instruction: 0xff7f3f07
        \\  ; <UNDEFINED> instruction: 0xfcfefeff
        \\rscshi r8, ip, #240 ; 0xf0
        \\  ; <UNDEFINED> instruction: 0x010101ff
        \\  ; <UNDEFINED> instruction: 0xf00f00cb
        \\  ; <UNDEFINED> instruction: 0xfefefcf8
        \\svcne 0x00ffff7f
        \\  ; <UNDEFINED> instruction: 0xffff7f3f
        \\ldclhi 15, cr15, [pc, #1008]! ; 0x22e8
        \\svccc 0x00ff0f00
        \\  ; <UNDEFINED> instruction: 0xfeffff3f
        \\ldrsheq pc, [pc], #140 ; <UNPREDICTABLE>
        \\svccc 0x007f7f60
        \\addseq r1, r1, pc, lsr pc
        \\sbc r8, r0, r3
        \\andhi r8, r0, #224 ; 0xe0
        \\  ; <UNDEFINED> instruction: 0x01008dff
        \\  ; <UNDEFINED> instruction: 0xfe80e0e0
        \\  ; <UNDEFINED> instruction: 0xf0ffff04
        \\svccc 0x0082070f
        \\addeq r0, sp, r0, lsl 0x6
        \\mvnshi pc, 0x1
        \\andhi r8, r3, #248 ; 0xf8
        \\strhi r8, [r0, #-3329] ; 0xfffff2ff
        \\  ; <UNDEFINED> instruction: 0x010101fc
        \\  ; <UNDEFINED> instruction: 0xf00100b4
        \\ldrsheq r8, [ip], #40 ; 0x28 ; <UNPREDICTABLE>
        \\mvnseq r8, #0, 2
        \\ldrsheq pc, [ip], #140 ; 0x8c ; <UNPREDICTABLE>
        \\orreq r0, r0, r1, lsl #15
        \\  ; <UNDEFINED> instruction: 0xfc070085
        \\  ; <UNDEFINED> instruction: 0xfcfcf8f8
        \\ldrshhi pc, [ip], #238 ; 0xee ; <UNPREDICTABLE>
        \\teqeq pc, pc ; <illegal shifter operand> ; <UNPREDICTABLE>
        \\cmneq pc, r1, lsl #30
        \\  ; <UNDEFINED> instruction: 0xfc03008c
        \\  ; <UNDEFINED> instruction: 0x803ffffe
        \\ldrshhi r0, [ip], #15 ; <UNPREDICTABLE>
        \\  ; <UNDEFINED> instruction: 0xfefc05ff
        \\  ; <UNDEFINED> instruction: 0x003fffff
        \\adceq r0, fp, r1, lsl 0x3
        \\  ; <UNDEFINED> instruction: 0xf881f000
        \\tsteq r1, #128 ; 0x80
        \\stchi 0, cr8, [r1, #-12]
        \\  ; <UNDEFINED> instruction: 0xf8f80100
        \\  ; <UNDEFINED> instruction: 0xfd00fc83
        \\vpmax.f32 d15, d19, d1
        \\strhi pc, [r1], #-241 ; 0xffffff0f
        \\andeq r8, r0, #12582912 ; 0xc00000
        \\ldrshhi pc, [lr, #236]! ; 0xec ; <UNPREDICTABLE>
        \\  ; <UNDEFINED> instruction: 0xf07f027e
        \\ldrshhi r8, [r8, #48]! ; 0x30
        \\stmdage r1, {r0, r1, r8, pc}
        \\rscseq r8, r8, r0, lsl 0x2
        \\tsteq r0, r0, ror r1
        \\addeq r0, pc, r1, lsl 0x2
        \\  ; <UNDEFINED> instruction: 0xfe80fc80
        \\ldrbls r7, [lr, #-3585]! ; 0xfffff1ff
        \\rpwvce f0, f6, f0
        \\svccc 0x00807f80
        \\umullhi r0, r1, r9, r0
        \\svceq 0x00030081
        \\mcrhi 15, 0, r1, cr15, cr15, {0}
        \\sbchi r8, r0, r0, lsl 0x2
        \\tsthi r7, #224 ; 0xe0
        \\stchi 0, cr0, [r7, #-60] ; 0xffffffc4
        \\mvnhi r8, r0, lsl 0x2
        \\  ; <UNDEFINED> instruction: 0x010783f0
        \\addeq r0, sp, r3, lsl 0x6
        \\  ; <UNDEFINED> instruction: 0xf8f0f007
        \\  ; <UNDEFINED> instruction: 0xfffffefe
        \\tsteq r3, pc, ror r1
        \\adcseq r0, r2, r1, lsl 0x2
        \\  ; <UNDEFINED> instruction: 0xfc81f800
        \\orreq r0, r0, r0, lsl 0x1
        \\  ; <UNDEFINED> instruction: 0xfc80008f
        \\rsbshi pc, lr, r1, lsl #28
        \\  ; <UNDEFINED> instruction: 0xfcf80afe
        \\svccc 0x007ffffe
        \\tsteq r7, #63, 30 ; 0xfc
        \\andeq r8, r0, #4096 ; 0x1000
        \\rscshi pc, pc, #1016 ; 0x3f8
        \\svcne 0x001f073f
        \\  ; <UNDEFINED> instruction: 0xfeff7f3f
        \\  ; <UNDEFINED> instruction: 0x0083fcfc
        \\stmdage r3, {r0, r8}
        \\mvnseq r8, r0
        \\  ; <UNDEFINED> instruction: 0x0080fcfc
        \\addeq r0, sp, r2, lsl 0x3
        \\  ; <UNDEFINED> instruction: 0xfe80fc81
        \\addseq r7, r5, r0, lsl #28
        \\svcvc 0x00017e80
        \\ldrle r8, [pc, #-127]! ; 0x1fc9
        \\  ; <UNDEFINED> instruction: 0xf8f80100
        \\  ; <UNDEFINED> instruction: 0xff82fc83
        \\  ; <UNDEFINED> instruction: 0xf8f8fb03
        \\ldrbteq r8, [pc], #508 ; 0x2058
        \\mvneq lr, r7, ror 0x7
        \\andeq r8, r7, #201326592 ; 0xc000000
        \\ldrshhi pc, [lr, #236]! ; 0xec ; <UNPREDICTABLE>
        \\cmnhi pc, lr, ror r0 ; <UNPREDICTABLE>
        \\mvnshi r8, #248, 2 ; 0x3e
        \\  ; <UNDEFINED> instruction: 0xf8f003f1
        \\orreq r0, r3, #1835008 ; 0x1c0000
        \\  ; <UNDEFINED> instruction: 0xf80100bd
        \\rscshi r8, ip, #248, 6 ; 0xe0000003
        \\  ; <UNDEFINED> instruction: 0xf0f103ff
        \\orreq r0, r4, #240, 2 ; 0x3c
        \\  ; <UNDEFINED> instruction: 0xfc020085
        \\mcrvc 14, 4, pc, cr1, cr14, {7} ; <UNPREDICTABLE>
        \\  ; <UNDEFINED> instruction: 0xf0f07f02
        \\orreq pc, r1, #8585216 ; 0x830000
        \\sbceq r0, r5, r1, lsl 0x3
        \\  ; <UNDEFINED> instruction: 0xfcf8e009
        \\svcvc 0x00fffefe
        \\rsbshi r3, pc, pc, lsr pc ; <UNPREDICTABLE>
        \\  ; <UNDEFINED> instruction: 0xf8fc02ff
        \\  ; <UNDEFINED> instruction: 0x800080f8
        \\tsteq r3, #1073741824 ; 0x40000000
        \\svccc 0x00100085
        \\  ; <UNDEFINED> instruction: 0xfeff7f3f
        \\  ; <UNDEFINED> instruction: 0xf8f0fcfe
        \\  ; <UNDEFINED> instruction: 0xfffffcf8
        \\tsteq pc, #508 ; 0x1fc
        \\addeq r0, pc, r0, lsl 0x3
        \\mvnhi lr, 0x1
        \\streq r0, [r7, -r0, lsl #2]
        \\add r0, r0, sp, lsl 0x1
        \\  ; <UNDEFINED> instruction: 0xf8f0f005
        \\andhi pc, r7, #252, 28 ; 0xfc0
        \\mvngt r0, #1069547520 ; 0x3fc00000
        \\streq r0, [r3, -r0, lsl #2]
        \\addeq r0, r5, r1, lsl #31
        \\  ; <UNDEFINED> instruction: 0xfffffe02
        \\tstpgt r3, #8519680 ; 0x820000
        \\mvnshi lr, r3, asr 0x7
        \\tsteq pc, #-1073741761 ; 0xc000003f
        \\tsteq r3, r7, lsl #14
        \\  ; <UNDEFINED> instruction: 0xfc840085
        \\mvnseq pc, r2, lsl #28
        \\strh r0, [r7], -r4
        \\  ; <UNDEFINED> instruction: 0xfefefcf8
        \\andhi r7, r0, #1020 ; 0x3fc
        \\  ; <UNDEFINED> instruction: 0xfcfc02ff
        \\strhi r8, [r1], -r0, lsl 0x6
        \\eorseq r8, pc, r0
        \\rscseq r8, pc, pc, ror r0 ; <UNPREDICTABLE>
        \\mvnseq r8, #-2147483585 ; 0x8000003f
        \\svcvc 0x007fffff
        \\  ; <UNDEFINED> instruction: 0xfc00008d
        \\svcvc 0x00010084
        \\rsbshi r8, pc, lr, ror r0 ; <UNPREDICTABLE>
        \\tsteq r0, pc, lsr sp
        \\  ; <UNDEFINED> instruction: 0xfe81fcfc
        \\cmnhi lr, r1, lsl #28
        \\lfmne f0, 4, [pc], {63} ; 0x3f
        \\tsteq r0, r8, lsl #28
        \\svccc 0x00837f7e
        \\  ; <UNDEFINED> instruction: 0xf80100d5
        \\ldrsheq r8, [lr, #44]! ; 0x2c
        \\svcne 0x00810ffc
        \\tsteq r1, #131072 ; 0x20000
        \\  ; <UNDEFINED> instruction: 0xf802008d
        \\  ; <UNDEFINED> instruction: 0xff81c3f0
        \\andhi pc, r7, #1, 28
        \\tsteq r7, #-1073741821 ; 0xc0000003
        \\  ; <UNDEFINED> instruction: 0xf08000b2
        \\streq r0, [r2, -r2, lsl #1]
        \\addeq r0, sp, r3, lsl 0x6
        \\rscshi pc, lr, r1, lsl #28
        \\  ; <UNDEFINED> instruction: 0xf8f802ff
        \\  ; <UNDEFINED> instruction: 0x001f81fc
        \\stchi 0, cr8, [r1, #-60] ; 0xffffffc4
        \\ldrbteq r8, [ip], #768 ; 0x300
        \\  ; <UNDEFINED> instruction: 0x0001f0f8
        \\sfmgt f0, 1, [pc, #-48] ; 0x2164
        \\  ; <UNDEFINED> instruction: 0xfefe0100
        \\svcvc 0x00027e81
        \\  ; <UNDEFINED> instruction: 0xf884f07f
        \\orreq r0, r2, r0, lsl 0x7
        \\svcvc 0x00020085
        \\  ; <UNDEFINED> instruction: 0xff803f3f
        \\  ; <UNDEFINED> instruction: 0xfcfefe04
        \\  ; <UNDEFINED> instruction: 0xff82fefc
        \\blgt 0x425bc
        \\andshi r0, pc, r0
        \\eorshi r0, lr, pc, lsr r0
        \\  ; <UNDEFINED> instruction: 0xfcfc08fe
        \\svccc 0x003f7efe
        \\stchi 15, cr1, [r1], {31}
        \\mvnshi r8, r0, lsl 0x2
        \\svceq 0x000f06f8
        \\tsteq r3, #1835008 ; 0x1c0000
        \\tsteq r0, r1, lsl #28
        \\svccc 0x00801f1f
        \\  ; <UNDEFINED> instruction: 0xfefebe05
        \\cmnhi lr, ip, lsr lr
        \\  ; <UNDEFINED> instruction: 0xf8ff0a7f
        \\mrcvc 12, 7, APSR_nzcv, cr14, cr12, {7}
        \\tsteq pc, pc, lsr pc ; <UNPREDICTABLE>
        \\andeq r8, r0, r1, lsl 0x6
        \\ldrshhi r8, [ip], #30 ; <UNPREDICTABLE>
        \\ldcleq 0, cr8, [pc], #992 ; 0x25e0
        \\  ; <UNDEFINED> instruction: 0xf1f1fbfb
        \\svceq 0x000f1ff0
        \\tsteq r3, r7, lsl #14
        \\beq 0x33614
        \\  ; <UNDEFINED> instruction: 0xf0f0f8f8
        \\sbchi ip, r0, r0, ror 0x1
        \\rscshi pc, fp, r1, ror 0x7
        \\svccc 0x007f06ff
        \\tsteq r7, #15, 30 ; 0x3c
        \\svceq 0x00008801
        \\  ; <UNDEFINED> instruction: 0xf8f0e0c0
        \\svcvc 0x00fefcfc
        \\  ; <UNDEFINED> instruction: 0xff7f7f3f
        \\  ; <UNDEFINED> instruction: 0xf8f8fdff
        \\tsteq r2, r2, lsl 0x1
        \\adceq r0, r6, r3, lsl 0x6
        \\eorshi r3, pc, r1, lsl #30
        \\  ; <UNDEFINED> instruction: 0xfcfc0e7e
        \\  ; <UNDEFINED> instruction: 0xf8f8f000
        \\mrcvc 12, 7, APSR_nzcv, cr14, cr12, {7}
        \\mrseq r0, SP_svc
        \\andeq r8, r0, #65536 ; 0x10000
        \\ldrshhi pc, [r8], #140 ; 0x8c ; <UNPREDICTABLE>
        \\  ; <UNDEFINED> instruction: 0xe0e009f0
        \\svcne 0x003f3f7f
        \\  ; <UNDEFINED> instruction: 0x070f0f1f
        \\  ; <UNDEFINED> instruction: 0xf00c008d
        \\  ; <UNDEFINED> instruction: 0xfcf8f8f0
        \\  ; <UNDEFINED> instruction: 0x077efefc
        \\tsteq r1, r3, lsl 0x6
        \\  ; <UNDEFINED> instruction: 0xfc8100d0
        \\addgt r0, r0, r3
        \\ldrbteq r8, [pc], r0, ror 0x1
        \\svcne 0x003f7f7f
        \\blhi 0x426c8
        \\  ; <UNDEFINED> instruction: 0xf0e00c00
        \\  ; <UNDEFINED> instruction: 0xfefcf8f8
        \\  ; <UNDEFINED> instruction: 0x070fffff
        \\rscshi r0, pc, r3, lsl 0x6
        \\tsteq r0, #-1073741793 ; 0xc000001f
        \\  ; <UNDEFINED> instruction: 0xf8f8f0e0
        \\svceq 0x00810081
        \\  ; <UNDEFINED> instruction: 0xf882008d
        \\  ; <UNDEFINED> instruction: 0xfffffc03
        \\cdphi 3, 0, cr8, cr1, cr15, {0}
        \\svcvc 0x00ff0400
        \\ldrshhi pc, [ip], #255 ; 0xff ; <UNPREDICTABLE>
        \\  ; <UNDEFINED> instruction: 0x810081f8
        \\tsthi r0, #1, 26 ; 0x40
        \\ldrsh r0, [r0], #24 ; <UNPREDICTABLE>
        \\svceq 0x00820180
        \\svcne 0x00810091
        \\svcne 0x00850095
        \\svcne 0x00850095
        \\svcne 0x00850095
        \\svcvc 0x00000099
        \\addseq pc, r5, r0, lsl #31
        \\mvnshi pc, #1, 30
        \\  ; <UNDEFINED> instruction: 0x010183f8
        \\addeq r0, sp, r7, lsl #30
        \\  ; <UNDEFINED> instruction: 0xf882f080
        \\tsteq r0, #516 ; 0x204
        \\addeq r0, sp, r0, lsl 0x3
        \\  ; <UNDEFINED> instruction: 0xfcf8f802
        \\svcvc 0x0002ff81
        \\sbcseq r0, r4, r1, lsl 0x2
        \\mvnshi pc, r1, lsl #16
        \\  ; <UNDEFINED> instruction: 0x000303ff
        \\  ; <UNDEFINED> instruction: 0xff828700
        \\andhi r0, r2, 0x1
        \\strgt r0, [r1, -r3]
        \\  ; <UNDEFINED> instruction: 0xffff0d00
        \\blcc 0x7c9f60
        \\  ; <UNDEFINED> instruction: 0xffff0000
        \\ldclle 8, cr15, [r8], #960 ; 0x3c0
        \\blvc 0x2c2564
        \\mvngt lr, #-872415231 ; 0xcc000001
        \\cdple 3, 14, cr14, cr3, cr3, {6}
        \\sbchi ip, r7, lr, asr #15
        \\stclhi 0, cr0, [r7, #780] ; 0x30c
        \\mvnsvc r0, #0, 30
        \\svceq 0x001f3f3b
        \\mcrgt 15, 6, pc, cr15, cr15, {7} ; <UNPREDICTABLE>
        \\  ; <UNDEFINED> instruction: 0xf0f8fcdc
        \\  ; <UNDEFINED> instruction: 0x0000ffff
        \\streq r0, [r1], #-128 ; 0xffffff80
        \\andeq r0, r0, r0
        \\nop   ; (mov r0, r0)
        \\nop   ; (mov r0, r0)
        \\tsteq sl, r9
        \\tsteq r5, r2, lsl r2
        \\tsteq sl, r3, lsl r1
        \\andeq r0, sl, #1073741830 ; 0x40000006
        \\  ; <UNDEFINED> instruction: 0xff0c020d
        \\andseq r0, r5, #536870913 ; 0x20000001
        \\tstpeq pc, ip, lsl #30
        \\  ; <UNDEFINED> instruction: 0xfe14000a
        \\ldreq r0, [r3, #-275] ; 0xfffffeed
        \\tsteq r3, r3, lsl r0
        \\tsteq r3, r3, lsl r0
        \\andseq r0, r3, #-1073741820 ; 0xc0000004
        \\tsteq r3, r3, lsl r0
        \\  ; <UNDEFINED> instruction: 0xff0c000a
        \\andseq r0, r5, #1342177281 ; 0x50000001
        \\tsteq lr, r5, lsl r2
        \\andseq r0, r6, pc, lsl r1
        \\tsteq r6, r6, lsl r1
        \\tsteq r4, r9, lsl r1
        \\tsteq r8, r2, lsl r1
        \\tsteq fp, fp, lsl r1
        \\tstpeq r9, lr, lsl #28
        \\eoreq r0, r1, r2, lsl r1
        \\tsteq sl, fp, lsl r1
        \\tsteq sl, r5, lsl r1
        \\andseq r0, r1, r7, lsl r1
        \\andseq r0, fp, #1879048193 ; 0x70000001
        \\  ; <UNDEFINED> instruction: 0x01220216
        \\andseq pc, r6, #27, 30 ; 0x6c
        \\andeq pc, lr, sl, lsl pc ; <UNPREDICTABLE>
        \\  ; <UNDEFINED> instruction: 0xfe0f010f
        \\andseq r0, r2, r5, lsl r2
        \\tsteq r3, fp, lsl 0x6
        \\tsteq r1, r3, lsl r0
        \\tsteq r2, r4, lsl r1
        \\andseq r0, r3, r0, lsl r0
        \\tsteq sl, r3, lsl r0
        \\tstpeq r4, sp, lsl #28
        \\andseq r0, ip, sl, lsl 0x2
        \\tsteq r3, r3, lsl r0
        \\tsteq r3, r5, lsl r0
        \\andeq r0, sp, pc, lsl 0x2
        \\tsteq r3, sp
        \\tsteq sl, r2, lsl r1
        \\andseq pc, r2, r5, lsl lr ; <UNPREDICTABLE>
        \\tsteq sp, r1, lsl r0
        \\tsteq sp, r9, lsl 0x6
        \\andseq r0, r0, r5, lsl r2
        \\mrcvc 14, 3, r7, cr8, cr9, {5}
        \\mrcvc 14, 1, r7, cr6, cr7, {2}
        \\ldclvc 14, cr7, [r5, #84] ; 0x54
        \\ldcvc 13, cr7, [r3, #720] ; 0x2d0
        \\ldcvc 13, cr7, [r1, #-456]! ; 0xfffffe38
        \\ldclvc 13, cr7, [r0], #68 ; 0x44
        \\stmvc lr, {r0, r1, r2, r3, r5, r7, sl, fp, ip, sp, lr}
        \\stmdavc sp, {r1, r2, r3, r5, r6, fp, ip, sp, lr}^
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\  ; <UNDEFINED> instruction: 0xf0000000
        \\  ; <UNDEFINED> instruction: 0xff000000
        \\cdp 0, 15, cr0, cr0, cr0, {0}
        \\cdple 0, 14, cr0, cr15, cr0, {0}
        \\stclgt 0, cr15, [pc] ; 0x2460
        \\ldclgt 0, cr15, [lr], {0}
        \\  ; <UNDEFINED> instruction: 0xfffff000
        \\mcr 15, 7, lr, cr14, cr0, {7}
        \\ldclle 14, cr13, [sp, #956] ; 0x3bc
        \\stclgt 13, cr13, [ip], {222} ; 0xde
        \\bllt 0xfeef57ec
        \\bge 0xfeab13ac
        \\ldmibls r9, {r2, r3, r4, r5, r7, r9, fp, sp, pc}
        \\stmhi r8, {r0, r1, r3, r5, r7, r9, fp, ip, pc}
        \\ldcllt 15, cr14, [sp], {0}
        \\blge 0xff37e08c
        \\blge 0xff37a050
        \\bls 0xfef3a054
        \\bls 0xfef3a058
        \\bls 0xfef3a05c
        \\bls 0xfef3a060
        \\bls 0xfef3a064
        \\  ; <UNDEFINED> instruction: 0x7778899a
        \\  ; <UNDEFINED> instruction: 0x6667789a
        \\ldrbpl r7, [r6, #-1929] ; 0xfffff877
        \\ldrbmi r6, [r5], #-1928 ; 0xfffff878
        \\cmpcc r5, #120, 12 ; 0x7800000
        \\eorscs r5, r4, #120, 12 ; 0x7800000
        \\eorsne r5, r4, #120, 12 ; 0x7800000
        \\eorsne r5, r4, #120, 12 ; 0x7800000
        \\  ; <UNDEFINED> instruction: 0xf8f8f8f8
        \\  ; <UNDEFINED> instruction: 0xf8f8f8f8
        \\ldrsheq pc, [pc], #255 ; <UNPREDICTABLE>
        \\svcvc 0x00000000
        \\ldclvc 13, cr7, [sp], #-500 ; 0xfffffe0c
        \\ldclvc 12, cr7, [ip], #-496 ; 0xfffffe10
        \\andeq r0, r0, r0
        \\cdpne 8, 1, cr15, cr12, cr0, {7}
        \\andeq r0, r0, r0
        \\cdple 7, 12, cr12, cr15, cr0, {0}
        \\andeq r0, r0, r0
        \\svchi 0x00cfff00
        \\andeq r0, r0, r0
        \\  ; <UNDEFINED> instruction: 0xf7e7c300
        \\andeq r0, r0, r0
        \\mvns pc, lr, lsl pc ; <UNPREDICTABLE>
        \\andeq r0, r0, r0
        \\  ; <UNDEFINED> instruction: 0xf1f1f8f8
        \\andeq r0, r0, r0
        \\  ; <UNDEFINED> instruction: 0xf1f1e0e0
        \\ldrsheq pc, [r0], #0 ; <UNPREDICTABLE>
        \\  ; <UNDEFINED> instruction: 0xf1f1f303
        \\andeq r0, r1, r1, lsl 0x2
        \\  ; <UNDEFINED> instruction: 0xf9f9f9f8
        \\andeq r0, r0, r0
        \\rsc lr, r0, r0, ror 0x1
        \\andeq r0, r0, r0
        \\mvngt pc, #-872415229 ; 0xcc000003
        \\andeq r0, r0, r0
        \\  ; <UNDEFINED> instruction: 0xfff3e1e1
        \\andeq r0, r0, r0
        \\andeq r0, r1, r3, lsl 0x2
        \\  ; <UNDEFINED> instruction: 0xf8f8f8f8
        \\  ; <UNDEFINED> instruction: 0xf8f8f8f8
        \\andeq r0, r0, pc, ror r0
        \\andeq r0, r0, r0
        \\ldclvc 12, cr7, [ip], #-496 ; 0xfffffe10
        \\ldclvc 12, cr7, [ip], #-496 ; 0xfffffe10
        \\svcne 0x001fff1e
        \\  ; <UNDEFINED> instruction: 0xf83c1e1e
        \\  ; <UNDEFINED> instruction: 0xc0c0dfde
        \\  ; <UNDEFINED> instruction: 0xc7cfdede
        \\svceq 0x000f0f0f
        \\svceq 0x000f0f0f
        \\  ; <UNDEFINED> instruction: 0xf0f0f0f0
        \\  ; <UNDEFINED> instruction: 0x80e0e0f0
        \\mvn lr, r1, ror 0x3
        \\svcvc 0x00f3e1e1
        \\mvngt lr, #-872415229 ; 0xcc000003
        \\addhi r8, r0, r3, asr 0x3
        \\blvc 0x1f01554
        \\svcne 0x001f3f3f
        \\  ; <UNDEFINED> instruction: 0xf0f0f0f0
        \\  ; <UNDEFINED> instruction: 0xf0f0f0f0
        \\  ; <UNDEFINED> instruction: 0xf9f9f9f9
        \\ldrsh pc, [r1, #153]! ; 0x99 ; <UNPREDICTABLE>
        \\rsc lr, r0, r0, ror 0x1
        \\  ; <UNDEFINED> instruction: 0xffe1e0e0
        \\tsthi r3, #201326594 ; 0xc000002
        \\  ; <UNDEFINED> instruction: 0xf3f3e3c3
        \\svcvc 0x003f3f7f
        \\ldrsh pc, [r1, #63]! ; 0x3f ; <UNPREDICTABLE>
        \\andeq r0, r0, r0
        \\tsteq r1, #0, 2
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\  ; <UNDEFINED> instruction: 0xfc000000
        \\andeq r0, r0, r0
        \\svcne 0x00000000
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\  ; <UNDEFINED> instruction: 0xf8000000
        \\andeq r0, r0, r0
        \\svcne 0x00000000
        \\andeq r0, r0, r0
        \\  ; <UNDEFINED> instruction: 0xfc000000
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\stcvc 0, cr0, [r0], {-0}
        \\andeq r0, r0, r0
        \\andhi r0, r0, r0
        \\andeq r0, r0, r0
        \\svceq 0x00000000
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\ldclvc 12, cr15, [ip], #-1008 ; 0xfffffc10
        \\ldclvc 12, cr7, [ip], #-496 ; 0xfffffe10
        \\  ; <UNDEFINED> instruction: 0xf8f8ff7f
        \\  ; <UNDEFINED> instruction: 0xf8f8f8f8
        \\mrseq r0, (UNDEF: 0)
        \\ldmdbvc r9!, {r0, r6, r7, r8, ip, sp, lr, pc}^
        \\andeq r0, r0, r0
        \\ldmdavc ip!, {r0, r1, r2, r8, r9, sl, fp, ip}
        \\andeq r0, r0, r0
        \\svcne 0x003fdf9f
        \\andeq r0, r0, r0
        \\ldclvc 15, cr7, [lr], #-124 ; 0xffffff84
        \\svccc 0x003efefc
        \\  ; <UNDEFINED> instruction: 0xf8fcfeff
        \\eorseq r3, r8, pc, lsr pc
        \\  ; <UNDEFINED> instruction: 0xff7f1f01
        \\  ; <UNDEFINED> instruction: 0x0000fcfc
        \\  ; <UNDEFINED> instruction: 0xfcfcfcfc
        \\andeq r0, r0, r0
        \\ldclvc 12, cr15, [ip], #496 ; 0x1f0
        \\andeq r0, r0, r0
        \\  ; <UNDEFINED> instruction: 0xf8f8ff7f
        \\ldclvc 12, cr7, [ip], #-496 ; 0xfffffe10
        \\  ; <UNDEFINED> instruction: 0xfd7c7c7c
        \\addhi r8, r0, r0, lsl 0x1
        \\  ; <UNDEFINED> instruction: 0xff808080
        \\svceq 0x000f0f0f
        \\svceq 0x000f0f0f
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\ldclvc 12, cr7, [ip], #-496 ; 0xfffffe10
        \\  ; <UNDEFINED> instruction: 0xfcfcfcfc
        \\  ; <UNDEFINED> instruction: 0xf8f8f8f8
        \\svceq 0x003f7fff
        \\ldclvc 13, cr15, [sp, #-1012]! ; 0xfffffc0c
        \\rscs r7, r0, r8, ror r8
        \\andeq r7, r0, pc, ror pc
        \\svceq 0x003c7878
        \\svcne 0x001f1f1f
        \\svcne 0x001f1f1f
        \\ldclvc 12, cr7, [ip], #-496 ; 0xfffffe10
        \\ldclvc 12, cr7, [ip], #-496 ; 0xfffffe10
        \\cdpeq 0, 0, cr0, cr2, cr0, {6}
        \\  ; <UNDEFINED> instruction: 0xf0fcfefe
        \\  ; <UNDEFINED> instruction: 0xf8f8fcff
        \\svcne 0x007fffff
        \\  ; <UNDEFINED> instruction: 0xfdfdfdfd
        \\  ; <UNDEFINED> instruction: 0xfcfcfcfd
        \\ldclvc 12, cr7, [ip], #-496 ; 0xfffffe10
        \\ldclvc 12, cr7, [ip], #-496 ; 0xfffffe10
        \\  ; <UNDEFINED> instruction: 0xf0f0f0f0
        \\  ; <UNDEFINED> instruction: 0xf0f0f0f0
        \\ldclvc 13, cr7, [sp, #-1012]! ; 0xfffffc0c
        \\ldclvc 13, cr7, [sp, #-500]! ; 0xfffffe0c
        \\  ; <UNDEFINED> instruction: 0x808080ff
        \\addhi r8, r0, r0, lsl 0x1
        \\svceq 0x000f0f0f
        \\svceq 0x000f0f0f
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\streq r0, [r1], #-256 ; 0xffffff00
        \\andhi r0, r0, r1
        \\nop   ; (mov r0, r0)
        \\nop   ; (mov r0, r0)
        \\streq r0, [r1], #-384 ; 0xfffffe80
        \\andhi r0, r0, r3
        \\nop   ; (mov r0, r0)
        \\nop   ; (mov r0, r0)
        \\strbpl r2, [r3, #-8]
        \\strmi r5, [sp, -ip, asr #8]!
        \\andeq r4, r0, r2, asr 0x2
        \\nop   ; (mov r0, r0)
        \\stmfd sp!, {lr}
        \\ldr r0, [pc, #276] ; 0x2890
        \\mov r1, #33554432 ; 0x2000000
        \\push {r2, r3, r4}
        \\ldr r2, [r0], 0x4
        \\lsr r2, r2, 0x8
        \\bl 0x63c
        \\mov r0, #83886080 ; 0x5000000
        \\ldrh r1, [pc, #252] ; 0x2894
        \\strh r1, [r0]
        \\ldrh r1, [pc, #248] ; 0x2898
        \\strh r1, [r0, #2]
        \\strh r1, [r0, #6]
        \\ldrh r1, [pc, #240] ; 0x289c
        \\strh r1, [r0, #4]
        \\strh r1, [r0, #8]
        \\mov r0, #67108864 ; 0x4000000
        \\ldrh r1, [pc, #228] ; 0x28a0
        \\strh r1, [r0]
        \\mov r1, 0x8
        \\strh r1, [r0, #4]
        \\ldrh r1, [pc, #216] ; 0x28a4
        \\strh r1, [r0, #74] ; 0x4a
        \\mov r1, 0x7
        \\strh r1, [r0, #8]
        \\ldr r0, [pc, #204] ; 0x28a8
        \\ldr r1, [pc, #204] ; 0x28ac
        \\mov r2, 0x8
        \\ldr r3, [r0], 0x4
        \\str r3, [r1], 0x4
        \\subs r2, r2, 0x1
        \\bgt 0x27e0
        \\ldr r1, [pc, #184] ; 0x28b0
        \\mov r2, #32
        \\ldr r3, [r0], 0x4
        \\str r3, [r1], 0x4
        \\subs r2, r2, 0x1
        \\bgt 0x27f8
        \\ldr r0, [pc, #164] ; 0x28b4
        \\ldr r1, [pc, #164] ; 0x28b8
        \\mov r2, 0x8
        \\str r1, [r0], 0x4
        \\subs r2, r2, 0x1
        \\bgt 0x2814
        \\mov r1, r0
        \\ldr r0, [pc, #144] ; 0x28bc
        \\ldr r2, [pc, #144] ; 0x28c0
        \\mov r3, 0x1
        \\push {r2, r3, r4, r5, r6, r7, r8, r9, sl, fp, ip}
        \\ldrh r3, [r2]
        \\bl 0x464
        \\subs r3, r3, 0x1
        \\ldreq r2, [pc, #124] ; 0x28c4
        \\beq 0x2830
        \\ldr ip, [pc, #120] ; 0x28c8
        \\ldr fp, [pc, #120] ; 0x28cc
        \\mov sl, #64 ; 0x40
        \\ldrb r4, [fp], 0x2
        \\mov r0, 0x0
        \\mov r3, 0x0
        \\ldrb r1, [fp, r3]
        \\rsb r1, sl, r1, lsl 0x1
        \\ldrsh r1, [ip, r1]
        \\add r0, r0, r1, asr 0x8
        \\and r1, r1, #255 ; 0xff
        \\add r0, r0, r1
        \\add r3, r3, 0x1
        \\cmp r3, r4
        \\blt 0x2860
        \\lsr r0, r0, 0x1
        \\rsb r0, r0, #120 ; 0x78
        \\b 0x28d0
        \\andeq r1, r0, r0, lsr 0x3
        \\andeq r7, r0, r3, lsr #24
        \\  ; <UNDEFINED> instruction: 0x00007fff
        \\andeq r6, r0, sl, asr ip
        \\andeq r9, r0, r0, asr 0x2
        \\andeq r3, r0, r1
        \\andeq r2, r0, r0, lsr 0x8
        \\streq r0, [r0, #-544] ; 0xfffffde0
        \\streq r7, [r1], -r0, lsl #31
        \\streq r4, [r0], -r0
        \\tstne r1, r1, lsl r1
        \\andeq r2, r0, r0, asr 0x9
        \\andeq r2, r0, r0, asr #14
        \\andeq r2, r0, r0, asr r7
        \\andeq r2, r0, r0, ror 0x6
        \\andeq r2, r0, r0, ror #14
        \\ldrb r1, [fp, #-1]
        \\mov r3, 0x0
        \\ldrb r2, [fp, r3]
        \\bl 0x29d0
        \\rsb r5, sl, r2, lsl 0x1
        \\ldrsh r5, [ip, r5]
        \\add r0, r0, r5, asr 0x8
        \\and r5, r5, #255 ; 0xff
        \\add r0, r0, r5
        \\add r3, r3, 0x1
        \\cmp r3, r4
        \\blt 0x28d8
        \\ldr r3, [pc, #108] ; 0x2974
        \\mov r4, 0x0
        \\mov r5, #1020 ; 0x3fc
        \\mov r6, #16384 ; 0x4000
        \\mov r2, r1
        \\tst r4, 0x2
        \\addne r2, r2, #16
        \\strh r2, [r3], 0x2
        \\orr r0, r6, r4, lsl #12
        \\tst r4, 0x1
        \\addne r0, r0, #16
        \\strh r0, [r3], 0x2
        \\ldrh r0, [pc, #64] ; 0x2978
        \\strh r0, [r3], 0x4
        \\add r4, r4, 0x1
        \\cmp r4, 0x4
        \\blt 0x2910
        \\ldr r0, [pc, #48] ; 0x297c
        \\mov r1, 0x1
        \\mov r3, 0x5
        \\mov r2, #16
        \\strh r1, [r0], 0x2
        \\add r1, r1, 0x1
        \\subs r2, r2, 0x1
        \\bgt 0x2954
        \\add r0, r0, #32
        \\subs r3, r3, 0x1
        \\bgt 0x2950
        \\b 0x2980
        \\streq r0, [r0, -r0, ror #7]
        \\  ; <UNDEFINED> instruction: 0x000017fc
        \\streq r0, [r0], -lr, lsl 0x7
        \\mov r8, #240 ; 0xf0
        \\ldr sl, [pc, #60] ; 0x29c8
        \\mov fp, 0x1
        \\strh fp, [sl], 0x2
        \\bl 0xbfc
        \\mov fp, 0x1
        \\strh fp, [sl]
        \\ldr r3, [pc, #40] ; 0x29cc
        \\mov r4, 0x4
        \\ldrh r1, [r3]
        \\add r1, r1, 0x4
        \\strh r1, [r3], 0x8
        \\subs r4, r4, 0x1
        \\bgt 0x29a4
        \\subs r8, r8, 0x4
        \\bgt 0x2990
        \\ldmfd sp!, {lr}
        \\bx lr
        \\streq r0, [r0], #-512 ; 0xfffffe00
        \\streq r0, [r0, -r2, ror #7]
        \\push {r0, r1, r2, r3, lr}
        \\push {r0, r1}
        \\mov r0, #33554432 ; 0x2000000
        \\sub r2, r2, #32
        \\add r0, r0, r2, lsl 0x7
        \\ldr r1, [pc, #52] ; 0x2a20
        \\add r1, r1, r3, lsl 0x9
        \\ldr r2, [pc, #48] ; 0x2a24
        \\bl 0x450
        \\pop {r0, r1}
        \\mov r2, #117440512 ; 0x7000000
        \\add r2, r2, r3, lsl 0x3
        \\orr r1, r1, #2048 ; 0x800
        \\strh r1, [r2], 0x2
        \\orr r0, r0, #32768 ; 0x8000
        \\strh r0, [r2], 0x2
        \\lsl r3, r3, 0x4
        \\strh r3, [r2], 0x2
        \\pop {r0, r1, r2, r3, lr}
        \\bx lr
        \\streq r0, [r1], -r0
        \\andeq r2, r0, r0, asr r3
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
        \\andeq r0, r0, r0
    , instr_strs.items);
}
