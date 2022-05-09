const std = @import("std");
const Self = @This();

pub fn init() Self {
    return .{};
}

test "static analysis" {
    std.testing.refAllDecls(@This());
}
