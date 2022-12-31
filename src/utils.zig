const std = @import("std");
const sdl = @import("c.zig").sdl;

pub fn Field(comptime ty: type, comptime mem: anytype) type {
    comptime {
        return std.meta.fieldInfo(ty, mem).type;
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

pub fn alignedCreate(
    self: std.mem.Allocator,
    comptime T: type,
    // null means naturally aligned
    comptime alignment: ?u29,
) std.mem.Allocator.Error!*align(alignment orelse @alignOf(T)) T {
    if (@sizeOf(T) == 0) return @as(*T, undefined);
    const slice = try self.allocAdvancedWithRetAddr(T, alignment, 1, @returnAddress());
    return &slice[0];
}

test "static analysis" {
    std.testing.refAllDecls(@This());
}
