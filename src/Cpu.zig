const std = @import("std");
const instr = @import("instr.zig");
const Bus = @import("Bus.zig");
const Self = @This();

const Reg0 = 0;
const Reg1 = 1;
const Reg2 = 2;
const Reg3 = 3;
const Reg4 = 4;
const Reg5 = 5;
const Reg6 = 6;
const Reg7 = 7;
const Reg8 = 8;
const Reg9 = 9;
const Reg10 = 10;
const Reg11 = 11;
const Reg12 = 12;
const Reg13 = 13;
const Reg14 = 14;
const Reg15 = 15;
const Reg8Fiq = 16;
const Reg9Fiq = 17;
const Reg10Fiq = 18;
const Reg11Fiq = 19;
const Reg12Fiq = 20;
const Reg13Fiq = 21;
const Reg14Fiq = 22;
const Reg13Svc = 23;
const Reg14Svc = 24;
const Reg13Abt = 25;
const Reg14Abt = 26;
const Reg13Irq = 27;
const Reg14Irq = 28;
const Reg13Und = 29;
const Reg14Und = 30;

const Cpsr = struct {
    signed: bool = false,
    zero: bool = false,
    carry: bool = false,
    overflow: bool = false,
    _: u20 = undefined,
    irq_disable: bool = false,
    fiq_disable: bool = false,
    state: enum(u1) { arm, thumb } = .arm,
    mode: enum(u5) {
        user = 0b10000,
        fiq = 0b10001,
        irq = 0b10010,
        svc = 0b10011,
        abt = 0b10111,
        und = 0b11011,
        system = 0b11111,
    } = .user,
};

regs: [31]u32 = [_]u32{0} ** 31,
cpsr: Cpsr = .{},
spsr_fiq: Cpsr = undefined,
spsr_svc: Cpsr = undefined,
spsr_abt: Cpsr = undefined,
spsr_irq: Cpsr = undefined,
spsr_und: Cpsr = undefined,
bus: Bus,

pub fn init() Self {
    return .{ .bus = Bus.init() };
}

pub fn deinit(self: Self) void {
    _ = self;
}

fn getRegPtr(self: *Self, reg: instr.Register) *u32 {
    const idx = switch (self.cpsr.mode) {
        .fiq => if (reg > 7 and reg < 15)
            reg + @intCast(usize, Reg8Fiq - Reg8)
        else
            reg,
        .svc => if (reg == 13 or reg == 14)
            reg + @intCast(usize, Reg13Svc - Reg13)
        else
            reg,
        .abt => if (reg == 13 or reg == 14)
            reg + @intCast(usize, Reg13Abt - Reg13)
        else
            reg,
        .irq => if (reg == 13 or reg == 14)
            reg + @intCast(usize, Reg13Irq - Reg13)
        else
            reg,
        .und => if (reg == 13 or reg == 14)
            reg + @intCast(usize, Reg13Und - Reg13)
        else
            reg,
        else => reg,
    };
    return &self.regs[idx];
}

fn getReg(self: *Self, reg: instr.Register) u32 {
    return self.getRegPtr(reg).*;
}

fn setReg(self: *Self, reg: instr.Register, n: u32) void {
    self.getRegPtr(reg).* = n;
}

fn readWord(self: *Self) u32 {
    return self.bus.readWord(self.getReg(15));
}

pub fn clock(self: *Self) void {
    const opcode = instr.parseOpcode(self.readWord());
    switch (opcode.instr) {
        else => std.debug.todo("unimplemented opcode"),
    }
}

// Logical Alu
fn alu(self: *Self, payload: instr.AluInstr) void {
    const op2 = switch (payload.op2) {
        .reg => |s| blk: {
            const reg = self.getReg(s.reg);
            const shift_by = switch (s.shift_by) {
                .imm => |imm| imm,
                .reg => |reg| self.getReg(reg),
            };
            break :blk switch (s.shift_type) {
                .lsl => .{
                    .val = reg << shift_by,
                    .carry = @as(bool, reg >> (32 - shift_by) & 0x1),
                },
                .lsr => .{
                    .val = reg >> shift_by,
                    .carry = @as(bool, reg >> shift_by & 0x1),
                },
                .asr => .{
                    .val = @bitCast(u32, @bitCast(i32, reg) >> shift_by),
                    .carry = @as(bool, reg >> shift_by & 0x1),
                },
                .ror => .{
                    .val = std.math.rotr(u32, reg, shift_by),
                    .carry = @as(bool, reg >> shift_by & 0x1),
                },
            };
        },
        .imm => |s| .{
            .val = std.math.rotr(u32, s.imm, s.rot * 2),
            .carry = @as(bool, s.imm >> s.rot & 0x1),
        },
    };
    const res = switch (payload.op) {
        .mov => op2.val,
        .mvn => ~op2.val,
        .orr => payload.rn | op2.val,
        .eor, .teq => payload.rn ^ op2.val,
        .and_, .tst => payload.rn & op2.val,
        .bic => payload.rn & ~op2.val,
        .add => payload.rn + op2.val,
        .adc => payload.rn + op2.val + self.cpsr.carry,
        .sub => payload.rn - op2.val,
        .sbc => payload.rn - op2.val + self.cpsr.carry - 1,
        .rsb => op2.val - payload.rn,
        .rsc => op2.val - payload.rn + self.cpsr.carry - 1,
        .cmp => payload.rn - op2.val,
        .cmn => payload.rn + op2.val,
    };

    if (payload.op != .tst or payload.op != .teq or payload.op != .cmp or payload.op != .cmn)
        self.setReg(payload.rd, res);

    if (payload.s and payload.rd != 15) {
        if (res == 0) self.cpsr.zero = true;
        blk: {
            if (payload.op.isLogical()) {
                switch (payload.op2) {
                    .reg => |reg| if (reg.shift_by == 0) break :blk,
                    .imm => |imm| if (imm.rot == 0) break :blk,
                }
            }
            self.cpsr.carry = op2.carry;
        }
        if (!payload.op.isLogical())
            self.cpsr.overflow = res >> 31 != payload.rd >> 31;
        self.cpsr.signed = @as(bool, res >> 31);
    }
}

test "reg access" {
    var cpu = Self.init();
    for (cpu.regs) |*p, i| {
        p.* = @intCast(u32, i);
    }
    try std.testing.expect(cpu.getReg(13) == Reg13);
    cpu.cpsr.mode = .fiq;
    try std.testing.expect(cpu.getReg(13) == Reg13Fiq);
    cpu.cpsr.mode = .irq;
    try std.testing.expect(cpu.getReg(13) == Reg13Irq);
    cpu.cpsr.mode = .svc;
    try std.testing.expect(cpu.getReg(13) == Reg13Svc);
    cpu.cpsr.mode = .abt;
    try std.testing.expect(cpu.getReg(13) == Reg13Abt);
    cpu.cpsr.mode = .und;
    try std.testing.expect(cpu.getReg(13) == Reg13Und);
    cpu.cpsr.mode = .system;
    try std.testing.expect(cpu.getReg(13) == Reg13);
}

test "static analysis" {
    std.testing.refAllDecls(@This());
}
