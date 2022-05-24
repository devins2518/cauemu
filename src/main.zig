const std = @import("std");
const sdl = @cImport({
    @cInclude("SDL.h");
});
const Emu = @import("Emu.zig");

pub fn main() anyerror!void {
    var gba = Emu.init();
    defer gba.deinit();

    var e: sdl.SDL_Event = undefined;
    main: while (true) {
        while ((sdl.SDL_PollEvent(&e) == 1)) {
            switch (e.type) {
                sdl.SDL_QUIT => break :main,
                else => {},
            }
        }
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
