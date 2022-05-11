const std = @import("std");
const Self = @This();
const instr = @import("instr.zig");

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

pub fn init() Self {
    return .{};
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

pub fn clock(self: *Self) void {
    _ = self;
    @panic("unimplemented");
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
