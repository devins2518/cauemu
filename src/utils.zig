const std = @import("std");
const sdl = @cImport({
    @cInclude("SDL.h");
});

pub fn Field(comptime ty: type, comptime mem: anytype) type {
    comptime {
        return std.meta.fieldInfo(ty, mem).field_type;
    }
}

pub fn sdlPanic() noreturn {
    std.debug.print("SDL ERROR: {s}", .{sdl.SDL_GetError()});
    std.process.exit(1);
}

pub fn prefetchWarn() void {
    std.log.warn(
        \\\ALU operation using R15 as operand, result may be invalid due to prefetching.
    , .{});
}

test "static analysis" {
    std.testing.refAllDecls(@This());
}
