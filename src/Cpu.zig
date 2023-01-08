const std = @import("std");
const instr = @import("instr.zig");
const utils = @import("utils.zig");
const Allocator = std.mem.Allocator;
const Bus = @import("Bus.zig");
const Self = @This();
const Instruction = instr.Instruction;
const Register = instr.Register;
const BarrelShifter = @import("BarrelShifter.zig");
const ResultC = BarrelShifter.ResultC;

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

const Cpsr = packed struct {
    mode: enum(u5) {
        user = 0b10000,
        fiq = 0b10001,
        irq = 0b10010,
        svc = 0b10011,
        abt = 0b10111,
        undef = 0b11011,
        system = 0b11111,
    } = .svc,
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
        .undef => if (reg.r == 13 or reg.r == 14)
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

fn getPCStoreValue(self: *Self) u32 {
    return self.getReg(Register.from(PC));
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

fn getSpsr(self: *Self) *Cpsr {
    return switch (self.cpsr.mode) {
        .user, .system => &self.cpsr,
        .fiq => &self.spsr_fiq,
        .svc => &self.spsr_svc,
        .abt => &self.spsr_abt,
        .irq => &self.spsr_irq,
        .undef => &self.spsr_und,
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

fn @"and"(self: *Self, payload: instr.AluInstr) void {
    const op2 = self.getOp2Payload(payload.op2);
    const result = self.getReg(payload.rn) & op2.imm;
    if (payload.rd.r == PC) {
        self.setPC(result);
    } else {
        self.setReg(payload.rd, result);
        if (payload.s) {
            self.setFlag(.signed, @truncate(u1, result >> 31) == 1);
            self.setFlag(.zero, result == 0);
            self.setFlag(.carry, op2.carry);
        }
    }
}

fn eor(self: *Self, payload: instr.AluInstr) void {
    const op2 = self.getOp2Payload(payload.op2);
    const result = self.getReg(payload.rn) ^ op2.imm;
    if (payload.rd.r == PC) {
        self.setPC(result);
    } else {
        self.setReg(payload.rd, result);
        if (payload.s) {
            self.setFlag(.signed, @truncate(u1, result >> 31) == 1);
            self.setFlag(.zero, result == 0);
            self.setFlag(.carry, op2.carry);
        }
    }
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
    const op2 = self.getOp2Payload(payload.op2);
    const result = @addWithOverflow(self.getReg(payload.rn), op2.imm);
    if (payload.rd.r == PC) {
        self.setPC(result[0]);
    } else {
        self.setReg(payload.rd, result[0]);
        if (payload.s) {
            self.setFlag(.signed, @truncate(u1, result[0] >> 31) == 1);
            self.setFlag(.zero, result[0] == 0);
            self.setFlag(.carry, op2.carry);
            self.setFlag(.overflow, result[1] == 1);
        }
    }
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
    const result = self.getOp2Payload(payload.op2);
    if (payload.rd.r == PC) {
        self.setPC(result.imm);
    } else {
        self.setReg(payload.rd, result.imm);
        if (payload.s) {
            self.setFlag(.signed, @truncate(u1, result.imm >> 31) == 1);
            self.setFlag(.zero, result.imm == 0);
            if (payload.op2 == .imm or
                (payload.op2 == .reg and payload.op2.reg.shift_by == .imm and payload.op2.reg.shift_by.imm != 0))
                self.setFlag(.carry, result.carry);
        }
    }
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
    const rn = self.getReg(payload.rn);
    const offset_addr = if (payload.u == .up)
        rn + payload.offset.imm
    else
        rn - payload.offset.imm;
    const address = if (payload.indexing == .pre)
        offset_addr
    else
        rn;
    const value = if (payload.rd.r == PC)
        self.getPCStoreValue()
    else
        self.getReg(payload.rd);
    if (payload.size == .byte)
        self.bus.writeByte(address, @truncate(u8, value))
    else
        self.bus.writeWord(address, value);
    if (payload.w) self.setReg(payload.rn, offset_addr);
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
    const op = payload.op.msr;
    const mask = blk: {
        var mask: u32 = 0x00000000;
        if (op.write_f)
            mask |= 0xFF000000;
        if (op.write_s)
            mask |= 0x00FF0000;
        if (op.write_x)
            mask |= 0x0000FF00;
        if (op.write_c)
            mask |= 0x000000FF;
        if (self.cpsr.mode == .user) mask &= 0xFF000000;
        break :blk mask;
    };
    const imm = mask & switch (op.src) {
        .imm => |imm| imm,
        .reg => |reg| self.getReg(reg),
    };
    const updated = @bitCast(Cpsr, imm);
    if ((self.cpsr.mode == .user or self.cpsr.mode == .system) and payload.source == .spsr)
        CpuLog.err("Attempting to write to SPSR in {s} mode", .{@tagName(self.cpsr.mode)});

    switch (payload.source) {
        .cpsr => self.cpsr = updated,
        .spsr => self.getSpsr().* = updated,
    }
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

fn getOp2Payload(self: *Self, op2: instr.Op2) ResultC {
    return switch (op2) {
        .imm => |imm| BarrelShifter.armExpandImmC(imm, self.cpsr.carry),
        .reg => |reg| blk: {
            const shift_by = switch (reg.shift_by) {
                .imm => |imm| imm,
                .reg => |shift_reg| self.getReg(shift_reg),
            };
            break :blk BarrelShifter.shiftC(self.getReg(reg.reg), reg.shift_type, @truncate(u6, shift_by), self.cpsr.carry);
        },
    };
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
