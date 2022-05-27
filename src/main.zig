const std = @import("std");
const sdl = @cImport({
    @cInclude("SDL.h");
});
const Emu = @import("Emu.zig");

const CLOCKS_PER_SEC_MHZ = 16.78;
const CLOCKS_PER_SEC_HZ = CLOCKS_PER_SEC_MHZ * 1000000.0;
const SECS_PER_CLOCK = 1 / CLOCKS_PER_SEC_HZ;
const NSECS_PER_CLOCK = @as(
    comptime_int,
    @floor(SECS_PER_CLOCK * @as(comptime_float, std.time.ns_per_s)),
);

pub fn main() anyerror!void {
    var gba = Emu.init();
    defer gba.deinit();

    var timer = try std.time.Timer.start();

    var e: sdl.SDL_Event = undefined;
    main: while (true) {
        while ((sdl.SDL_PollEvent(&e) == 1)) {
            switch (e.type) {
                sdl.SDL_QUIT => break :main,
                else => {},
            }
        }
        gba.clock();
        var sleep = timer.previous.since(timer.started);
        std.time.sleep(NSECS_PER_CLOCK -| sleep);
    }
}

test "static analysis" {
    _ = @import("Bus.zig");
    _ = @import("Cpu.zig");
    _ = @import("Emu.zig");
    _ = @import("Ppu.zig");
    _ = @import("instr.zig");
    _ = @import("utils.zig");
    std.testing.refAllDecls(@This());
}
