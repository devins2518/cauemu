const std = @import("std");
const Self = @This();

pub fn init() Self {
    return .{};
}

pub fn deinit(self: Self) void {
    _ = self;
}

test "static analysis" {
    std.testing.refAllDecls(@This());
}
