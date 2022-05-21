const std = @import("std");

pub fn Field(comptime ty: type, comptime mem: anytype) type {
    comptime {
        return std.meta.fieldInfo(ty, mem).field_type;
    }
}

test "static analysis" {
    std.testing.refAllDecls(@This());
}
