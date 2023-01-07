const std = @import("std");
const instr = @import("instr.zig");
const utils = @import("utils.zig");
const Allocator = std.mem.Allocator;
const Bus = @import("Bus.zig");
const Self = @This();
const Instruction = instr.Instruction;
const Register = instr.Register;

const CpuLog = std.log.scoped(.Cpu);

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

prefetch_buffer: [2]?u32 = .{ null, null },
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
        .fiq => if (reg.r > 7 and reg.r < 15)
            reg.r + @intCast(usize, Reg8Fiq - Reg8)
        else
            reg.r,
        .svc => if (reg.r == 13 or reg.r == 14)
            reg.r + @intCast(usize, Reg13Svc - Reg13)
        else
            reg.r,
        .abt => if (reg.r == 13 or reg.r == 14)
            reg.r + @intCast(usize, Reg13Abt - Reg13)
        else
            reg.r,
        .irq => if (reg.r == 13 or reg.r == 14)
            reg.r + @intCast(usize, Reg13Irq - Reg13)
        else
            reg.r,
        .und => if (reg.r == 13 or reg.r == 14)
            reg.r + @intCast(usize, Reg13Und - Reg13)
        else
            reg.r,
        else => reg.r,
    };
    return &self.regs[idx];
}

fn getReg(self: *Self, reg: Register) u32 {
    return self.getRegPtr(reg).*;
}

fn setFlag(self: *Self, comptime flag: enum { overflow, zero, carry, signed }, update: bool) void {
    @field(self.cpsr, @tagName(flag)) = update;
}

fn setReg(self: *Self, reg: instr.Register, n: u32) void {
    self.getRegPtr(reg).* = n;
}

fn setPC(self: *Self, n: u32) void {
    self.branchWritePC(n);
}

fn branchWritePC(self: *Self, n: u32) void {
    if (self.cpsr.state == .arm) {
        if (@truncate(u2, n) != 0b00)
            CpuLog.warn("Unpredicatable: 0x{x:0>8} does not end in 0b00", .{n});
        self.branchTo(n & 0xFFFFFFFC);
    } else {
        self.branchTo(n & 0xFFFFFFFE);
    }
    self.flushPrefetch();
}

fn branchTo(self: *Self, n: u32) void {
    self.getRegPtr(Register.from(PC)).* = n;
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
    return self.bus.readWord(self.getReg(Register.from(PC)));
}

pub fn clock(self: *Self) !void {
    const instruction = self.fetch() orelse return;
    CpuLog.info("0x{x:0>8}: {}", .{ self.getReg(Register.from(PC)) - 12, instruction });
    switch (instruction) {
        .branch => |branch_payload| switch (branch_payload.op) {
            .b => self.branch(branch_payload),
            .bl => self.branchLink(branch_payload),
        },
        .branchx => |branchx_payload| self.branchEx(branchx_payload),
        .svc => |svc_payload| self.svc(svc_payload),
        .psr_transfer => |psr_payload| switch (psr_payload.op) {
            .mrs => self.mrs(psr_payload),
            .msr => self.msr(psr_payload),
        },
        .undef => self.undef(),
        inline else => |payload| switch (payload.op) {
            inline else => |op| @field(self, @tagName(op))(payload),
        },
    }
}

fn flushPrefetch(self: *Self) void {
    self.prefetch_buffer = .{ null, null };
}

fn fetch(self: *Self) ?Instruction {
    const opcode = if (self.prefetch_buffer[0]) |op|
        Instruction.parse(op, self.getReg(Register.from(PC)) - 8)
    else
        null;
    self.prefetch_buffer[0] = self.prefetch_buffer[1];
    self.prefetch_buffer[1] = self.readWord();
    self.getRegPtr(Register.from(PC)).* += 4;
    return opcode;
}

// Logical/Arithmetic Alu
fn alu(self: *Self, payload: instr.AluInstr) void {
    if (payload.rn.r == PC) utils.prefetchWarn();
    if (payload.rd.r == PC) utils.prefetchWarn();
    const rn = self.getReg(payload.rn);
    const op2: struct { val: u32, carry: bool } = blk: {
        switch (payload.op2) {
            .reg => |s| {
                const rm = self.getReg(s.reg);
                if (s.reg.r == PC) utils.prefetchWarn();
                switch (s.shift_by) {
                    // LSL#0
                    .imm => |_imm| {
                        const imm = @truncate(u5, _imm);
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
                        if (rs.r == PC) utils.prefetchWarn();
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
        .orr => rn | op2.val,
        .eor, .teq => rn ^ op2.val,
        .@"and", .tst => rn & op2.val,
        .bic => rn & ~op2.val,
        .add => rn + op2.val,
        .adc => rn + op2.val + @boolToInt(self.cpsr.carry),
        .sub => rn - op2.val,
        .sbc => rn - op2.val + @boolToInt(self.cpsr.carry) - 1,
        .rsb => op2.val - rn,
        .rsc => op2.val - rn + @boolToInt(self.cpsr.carry) - 1,
        .cmp => rn - op2.val,
        .cmn => rn + op2.val,
    };

    if (payload.op != .tst or payload.op != .teq or payload.op != .cmp or payload.op != .cmn)
        self.setReg(payload.rd, res);

    if (payload.s and payload.rd.r != 15) {
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

fn @"and"(self: *Self, payload: instr.AluInstr) void {
    _ = payload;
    _ = self;
    CpuLog.err("unimplemented instruction", .{});
}

fn eor(self: *Self, payload: instr.AluInstr) void {
    _ = payload;
    _ = self;
    CpuLog.err("unimplemented instruction", .{});
}

fn sub(self: *Self, payload: instr.AluInstr) void {
    _ = payload;
    _ = self;
    CpuLog.err("unimplemented instruction", .{});
}

fn rsb(self: *Self, payload: instr.AluInstr) void {
    _ = payload;
    _ = self;
    CpuLog.err("unimplemented instruction", .{});
}

fn add(self: *Self, payload: instr.AluInstr) void {
    _ = payload;
    _ = self;
    CpuLog.err("unimplemented instruction", .{});
}

fn adc(self: *Self, payload: instr.AluInstr) void {
    _ = payload;
    _ = self;
    CpuLog.err("unimplemented instruction", .{});
}

fn sbc(self: *Self, payload: instr.AluInstr) void {
    _ = payload;
    _ = self;
    CpuLog.err("unimplemented instruction", .{});
}

fn rsc(self: *Self, payload: instr.AluInstr) void {
    _ = payload;
    _ = self;
    CpuLog.err("unimplemented instruction", .{});
}

fn tst(self: *Self, payload: instr.AluInstr) void {
    _ = payload;
    _ = self;
    CpuLog.err("unimplemented instruction", .{});
}

fn teq(self: *Self, payload: instr.AluInstr) void {
    _ = payload;
    _ = self;
    CpuLog.err("unimplemented instruction", .{});
}

fn cmp(self: *Self, payload: instr.AluInstr) void {
    _ = payload;
    _ = self;
    CpuLog.err("unimplemented instruction", .{});
}

fn cmn(self: *Self, payload: instr.AluInstr) void {
    _ = payload;
    _ = self;
    CpuLog.err("unimplemented instruction", .{});
}

fn orr(self: *Self, payload: instr.AluInstr) void {
    _ = payload;
    _ = self;
    CpuLog.err("unimplemented instruction", .{});
}

fn mov(self: *Self, payload: instr.AluInstr) void {
    _ = payload;
    _ = self;
    CpuLog.err("unimplemented instruction", .{});
}

fn bic(self: *Self, payload: instr.AluInstr) void {
    _ = payload;
    _ = self;
    CpuLog.err("unimplemented instruction", .{});
}

fn mvn(self: *Self, payload: instr.AluInstr) void {
    _ = payload;
    _ = self;
    CpuLog.err("unimplemented instruction", .{});
}

fn mul(self: *Self, payload: instr.MulInstr) void {
    _ = payload;
    _ = self;
    CpuLog.err("unimplemented instruction", .{});
}

fn mla(self: *Self, payload: instr.MulInstr) void {
    _ = payload;
    _ = self;
    CpuLog.err("unimplemented instruction", .{});
}

fn umull(self: *Self, payload: instr.MulInstr) void {
    _ = payload;
    _ = self;
    CpuLog.err("unimplemented instruction", .{});
}

fn umlal(self: *Self, payload: instr.MulInstr) void {
    _ = payload;
    _ = self;
    CpuLog.err("unimplemented instruction", .{});
}

fn smull(self: *Self, payload: instr.MulInstr) void {
    _ = payload;
    _ = self;
    CpuLog.err("unimplemented instruction", .{});
}

fn smlal(self: *Self, payload: instr.MulInstr) void {
    _ = payload;
    _ = self;
    CpuLog.err("unimplemented instruction", .{});
}

fn str(self: *Self, payload: instr.SDTransferInstr) void {
    _ = payload;
    _ = self;
    CpuLog.err("unimplemented instruction", .{});
}

fn ldr(self: *Self, payload: instr.SDTransferInstr) void {
    _ = payload;
    _ = self;
    CpuLog.err("unimplemented instruction", .{});
}

fn ldrh(self: *Self, payload: instr.HSDTransferInstr) void {
    _ = payload;
    _ = self;
    CpuLog.err("unimplemented instruction", .{});
}

fn strh(self: *Self, payload: instr.HSDTransferInstr) void {
    _ = payload;
    _ = self;
    CpuLog.err("unimplemented instruction", .{});
}

fn ldrsb(self: *Self, payload: instr.HSDTransferInstr) void {
    _ = payload;
    _ = self;
    CpuLog.err("unimplemented instruction", .{});
}

fn ldrsh(self: *Self, payload: instr.HSDTransferInstr) void {
    _ = payload;
    _ = self;
    CpuLog.err("unimplemented instruction", .{});
}

fn stm(self: *Self, payload: instr.BDTransferInstr) void {
    _ = payload;
    _ = self;
    CpuLog.err("unimplemented instruction", .{});
}

fn ldm(self: *Self, payload: instr.BDTransferInstr) void {
    _ = payload;
    _ = self;
    CpuLog.err("unimplemented instruction", .{});
}

fn swp(self: *Self, payload: instr.SDSwapInstr) void {
    _ = payload;
    _ = self;
    CpuLog.err("unimplemented instruction", .{});
}

fn branch(self: *Self, payload: instr.BranchInstr) void {
    // BranchInstr already includes PC offset
    self.setPC(payload.offset);
}

fn branchLink(self: *Self, payload: instr.BranchInstr) void {
    _ = payload;
    _ = self;
    CpuLog.err("unimplemented instruction", .{});
}

fn branchEx(self: *Self, payload: instr.BranchExInstr) void {
    _ = payload;
    _ = self;
    CpuLog.err("unimplemented instruction", .{});
}

fn msr(self: *Self, payload: instr.PSRTransferInstr) void {
    _ = payload;
    _ = self;
    CpuLog.err("unimplemented instruction", .{});
}

fn mrs(self: *Self, payload: instr.PSRTransferInstr) void {
    _ = payload;
    _ = self;
    CpuLog.err("unimplemented instruction", .{});
}

fn svc(self: *Self, payload: instr.SVCInstr) void {
    _ = payload;
    _ = self;
    CpuLog.err("unimplemented instruction", .{});
}

fn undef(self: *Self) void {
    _ = self;
    CpuLog.err("unimplemented instruction", .{});
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
        p.* = @truncate(u32, i);
    }
    try std.testing.expectEqual(cpu.getReg(Register.from(13)), Reg13);
    cpu.cpsr.mode = .fiq;
    try std.testing.expectEqual(cpu.getReg(Register.from(13)), Reg13Fiq);
    cpu.cpsr.mode = .irq;
    try std.testing.expectEqual(cpu.getReg(Register.from(13)), Reg13Irq);
    cpu.cpsr.mode = .svc;
    try std.testing.expectEqual(cpu.getReg(Register.from(13)), Reg13Svc);
    cpu.cpsr.mode = .abt;
    try std.testing.expectEqual(cpu.getReg(Register.from(13)), Reg13Abt);
    cpu.cpsr.mode = .und;
    try std.testing.expectEqual(cpu.getReg(Register.from(13)), Reg13Und);
    cpu.cpsr.mode = .system;
    try std.testing.expectEqual(cpu.getReg(Register.from(13)), Reg13);
}

test "static analysis" {
    std.testing.refAllDecls(@This());
}
