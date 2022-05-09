const std = @import("std");
const Bus = @import("Bus.zig");
const Cpu = @import("Cpu.zig");
const Ppu = @import("Ppu.zig");
const Self = @This();

const ClocksPerSec = 16780000;

bus: Bus,
cpu: Cpu,
ppu: Ppu,

pub fn init() Self {
    const bus = Bus.init();
    const cpu = Cpu.init();
    const ppu = Ppu.init();

    return .{ .bus = bus, .cpu = cpu, .ppu = ppu };
}

test "static analysis" {
    std.testing.refAllDecls(@This());
}
