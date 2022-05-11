const std = @import("std");
const Emu = @import("Emu.zig");

pub fn main() anyerror!void {
    var gba = Emu.init();
    defer gba.deinit();
    _ = gba;
}

test "static analysis" {
    _ = @import("Bus.zig");
    _ = @import("Cpu.zig");
    _ = @import("Emu.zig");
    _ = @import("Ppu.zig");
    _ = @import("instr.zig");
    std.testing.refAllDecls(@This());
}
