const std = @import("std");
const sdl = @import("c.zig").sdl;
const utils = @import("utils.zig");
const Allocator = std.mem.Allocator;
const Bus = @import("Bus.zig");
const Cpu = @import("Cpu.zig");
const Ppu = @import("Ppu.zig");
const Self = @This();

const ClocksPerSec = 16780000;

gpa: std.heap.GeneralPurposeAllocator(.{}),

bus: *Bus,
cpu: *Cpu,
ppu: *Ppu,

pub fn init() !Self {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var alloc = gpa.allocator();
    var bus = try Bus.init(alloc);
    const cpu = try Cpu.init(alloc, bus);
    const ppu = try Ppu.init(alloc, bus);
    bus.registerPpu(ppu);
    ppu.reset();

    if (sdl.SDL_Init(sdl.SDL_INIT_EVERYTHING) != 0) utils.sdlPanic();

    return Self{ .gpa = gpa, .bus = bus, .cpu = cpu, .ppu = ppu };
}

pub fn clock(self: *Self) void {
    self.cpu.clock();
    self.ppu.clock();
}

pub fn deinit(self: *Self) void {
    const alloc = self.gpa.allocator();
    self.bus.deinit(alloc);
    alloc.destroy(self.bus);
    self.cpu.deinit();
    alloc.destroy(self.cpu);
    self.ppu.deinit();
    alloc.destroy(self.ppu);
    std.debug.assert(!self.gpa.deinit());
}

test "static analysis" {
    std.testing.refAllDecls(@This());
}
