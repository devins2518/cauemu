const std = @import("std");
const instr = @import("instr.zig");
const utils = @import("utils.zig");
const Allocator = std.mem.Allocator;
const Bus = @import("Bus.zig");
const Self = @This();
const Instruction = instr.Instruction;

const SP = 13;
const LR = 14;
const PC = 15;
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
    mode: enum(u5) {
        user = 0b10000,
        fiq = 0b10001,
        irq = 0b10010,
        svc = 0b10011,
        abt = 0b10111,
        und = 0b11011,
        system = 0b11111,
    } = .user,
    state: enum(u1) { arm, thumb } = .arm,
    fiq_disable: bool = false,
    irq_disable: bool = false,
    _: u20 = undefined,
    overflow: bool = false,
    carry: bool = false,
    zero: bool = false,
    signed: bool = false,
};

regs: [31]u32 = [_]u32{0} ** 31,
cpsr: Cpsr = .{},
spsr_fiq: Cpsr = undefined,
spsr_svc: Cpsr = undefined,
spsr_abt: Cpsr = undefined,
spsr_irq: Cpsr = undefined,
spsr_und: Cpsr = undefined,
bus: *Bus,

pub fn init(alloc: Allocator, bus: *Bus) !*Self {
    var self = try alloc.create(Self);
    self.* = .{ .bus = bus };
    return self;
}

pub fn deinit(self: *Self, allocator: Allocator) void {
    allocator.destroy(self);
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

fn getSpsr(self: *Self) Cpsr {
    return switch (self.cpsr.mode) {
        .user => self.cpsr,
        .fiq => self.spsr_fiq,
        .system => self.spsr_svc,
        .abt => self.spsr_abt,
        .irq => self.spsr_irq,
        .undef => self.spsr_und,
    };
}

fn readWord(self: *Self) u32 {
    return self.bus.readWord(self.getReg(PC));
}

pub fn clock(self: *Self) !void {
    const opcode = Instruction.parse(self.readWord(), self.getReg(PC));
    try std.fmt.format(std.io.getStdOut().writer(), "{}", .{opcode});
    switch (opcode.instr) {
        // Logical ALU
        .mov => |mov| self.alu(mov),
        .mvn => |mvn| self.alu(mvn),
        .orr => |orr| self.alu(orr),
        .eor => |eor| self.alu(eor),
        .@"and" => |@"and"| self.alu(@"and"),
        .bic => |bic| self.alu(bic),
        .tst => |tst| self.alu(tst),
        .teq => |teq| self.alu(teq),
        // Arithmetic ALU
        .add => |add| self.alu(add),
        .adc => |adc| self.alu(adc),
        .sub => |sub| self.alu(sub),
        .sbc => |sbc| self.alu(sbc),
        .rsb => |rsb| self.alu(rsb),
        .rsc => |rsc| self.alu(rsc),
        .cmp => |cmp| self.alu(cmp),
        .cmn => |cmn| self.alu(cmn),
        // Jump
        .b => |b| self.branch(b),
        .bl => |bl| self.branch(bl),
        else => {
            std.log.err("unimplemented opcode {}", .{opcode.instr});
            std.process.exit(1);
        },
    }
}

// Logical/Arithmetic Alu
fn alu(self: *Self, payload: instr.AluInstr) void {
    if (payload.rn == PC) utils.prefetchWarn();
    if (payload.rd == PC) utils.prefetchWarn();
    const op2: struct { val: u32, carry: bool } = blk: {
        switch (payload.op2) {
            .reg => |s| {
                const rm = self.getReg(s.reg);
                if (rm == PC) utils.prefetchWarn();
                switch (s.shift_by) {
                    // LSL#0
                    .imm => |imm| {
                        switch (s.shift_type) {
                            .lsl => if (imm == 0) {
                                break :blk .{ .val = rm, .carry = self.cpsr.carry };
                            } else {
                                break :blk .{
                                    .val = rm << imm,
                                    .carry = rm >> (31 - imm) & 0x1 == 0x1,
                                };
                            },
                            .lsr => break :blk .{
                                .val = rm >> imm,
                                .carry = rm >> imm & 0x1 == 0x1,
                            },
                            .asr => break :blk .{
                                .val = @intCast(u32, @bitCast(i32, rm) >> imm),
                                .carry = rm >> imm & 0x1 == 0x1,
                            },
                            .ror => break :blk .{
                                .val = std.math.rotr(u32, rm, @intCast(u8, imm)),
                                .carry = rm >> (imm - 1) & 0x1 == 0x1,
                            },
                        }
                    },
                    .reg => |rs| {
                        if (rs == PC) utils.prefetchWarn();
                        const shift = @truncate(u8, self.getReg(rs));
                        const shift_u5 = @truncate(u5, shift);
                        switch (s.shift_type) {
                            .lsl => if (shift == 32) {
                                break :blk .{ .val = 0, .carry = rm & 0x1 == 0x1 };
                            } else if (shift > 32) {
                                break :blk .{ .val = 0, .carry = false };
                            } else {
                                break :blk .{
                                    .val = rm << shift_u5,
                                    .carry = rm >> (31 - shift_u5) & 0x1 == 0x1,
                                };
                            },
                            .lsr => if (shift == 32) {
                                break :blk .{ .val = 0, .carry = rm >> 31 & 0x1 == 0x1 };
                            } else if (shift > 32) {
                                break :blk .{ .val = 0, .carry = false };
                            } else {
                                break :blk .{
                                    .val = rm >> shift_u5,
                                    .carry = rm >> shift_u5 & 0x1 == 0x1,
                                };
                            },
                            .asr => if (shift >= 32) {
                                break :blk .{
                                    .val = @intCast(u32, @bitCast(i32, rm) >> 31),
                                    .carry = rm >> 31 & 0x1 == 0x1,
                                };
                            } else {
                                break :blk .{
                                    .val = @intCast(u32, @bitCast(i32, rm) >> shift_u5),
                                    .carry = rm >> shift_u5 & 0x1 == 0x1,
                                };
                            },
                            .ror => if (shift == 32) {
                                break :blk .{ .val = rm, .carry = rm >> 31 & 0x1 == 0x1 };
                            } else if (shift == 0) {
                                break :blk .{
                                    .val = @intCast(u32, @boolToInt(self.cpsr.carry)) << 31 |
                                        rm >> 1,
                                    .carry = rm & 0x1 == 0x1,
                                };
                            } else if (shift > 32) {
                                // TODO carry might be off
                                break :blk .{
                                    .val = std.math.rotr(u32, rm, @mod(shift, 32)),
                                    .carry = rm >> (shift_u5 - 1) & 0x1 == 0x1,
                                };
                            } else {
                                break :blk .{
                                    .val = std.math.rotr(u32, rm, shift),
                                    .carry = rm >> (shift_u5 - 1) & 0x1 == 0x1,
                                };
                            },
                        }
                    },
                }
            },
            .imm => |s| break :blk .{
                .val = s,
                // TODO: accurate?
                .carry = @truncate(u1, s) == 0x1,
            },
        }
    };
    const res = switch (payload.op) {
        .mov => op2.val,
        .mvn => ~op2.val,
        .orr => payload.rn | op2.val,
        .eor, .teq => payload.rn ^ op2.val,
        .@"and", .tst => payload.rn & op2.val,
        .bic => payload.rn & ~op2.val,
        .add => payload.rn + op2.val,
        .adc => payload.rn + op2.val + @boolToInt(self.cpsr.carry),
        .sub => payload.rn - op2.val,
        .sbc => payload.rn - op2.val + @boolToInt(self.cpsr.carry) - 1,
        .rsb => op2.val - payload.rn,
        .rsc => op2.val - payload.rn + @boolToInt(self.cpsr.carry) - 1,
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
                    .reg => |reg| switch (reg.shift_by) {
                        .imm => |val| if (val == 0) break :blk,
                        .reg => |val| if (self.getReg(val) == 0) break :blk,
                    },
                    else => {},
                }
            }
            self.cpsr.carry = op2.carry;
        }
        if (!payload.op.isLogical())
            self.cpsr.overflow = res >> 31 != self.getReg(payload.rd) >> 31;
        self.cpsr.signed = res >> 31 & 0x1 == 0x1;
    }
}

fn branch(self: *Self, payload: instr.BranchInstr) void {
    if (payload.op == .bl) self.setReg(LR, self.getReg(PC));
    self.setReg(PC, self.getReg(PC) +% payload.offset);
}

test "reg access" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer std.debug.assert(!gpa.deinit());
    var bus = try Bus.init(allocator);
    defer bus.deinit(allocator);
    var cpu = try Self.init(allocator, bus);
    defer cpu.deinit(allocator);
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
