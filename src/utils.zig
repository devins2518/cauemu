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

test "static analysis" {
    std.testing.refAllDecls(@This());
}
