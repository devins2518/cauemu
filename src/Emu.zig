const std = @import("std");
const sdl = @cImport({
    @cInclude("SDL.h");
});
const utils = @import("utils.zig");
const Bus = @import("Bus.zig");
const Cpu = @import("Cpu.zig");
const Ppu = @import("Ppu.zig");
const Self = @This();

const ClocksPerSec = 16780000;

bus: Bus,
cpu: Cpu,
ppu: Ppu,

pub fn init() Self {
    var bus = Bus.init();
    const cpu = Cpu.init();
    const ppu = Ppu.init(&bus);

    if (sdl.SDL_Init(sdl.SDL_INIT_EVERYTHING) != 0) utils.sdlPanic();

    return .{ .bus = bus, .cpu = cpu, .ppu = ppu };
}

pub fn clock(self: *Self) void {
    self.cpu.clock();
    self.ppu.clock();
}

pub fn deinit(self: Self) void {
    self.bus.deinit();
    self.cpu.deinit();
    self.ppu.deinit();
}

test "static analysis" {
    std.testing.refAllDecls(@This());
}
