const std = @import("std");
const Self = @This();

pub fn init() Self {
    return .{};
}

pub fn readByte(self: *Self, addr: u32) u8 {
    return self.read(addr, u8);
}
pub fn readHalfWord(self: *Self, addr: u32) u16 {
    return self.read(addr, u16);
}
pub fn readWord(self: *Self, addr: u32) u32 {
    return self.read(addr, u32);
}

fn read(self: *Self, addr: u32, comptime T: type) T {
    _ = self;
    _ = addr;
    @panic("Unimplemented read");
}

pub fn writeByte(self: *Self, addr: u32, n: u8) void {
    self.write(addr, u8, n);
}
pub fn writeHalfWord(self: *Self, addr: u32, n: u16) void {
    self.write(addr, u16, n);
}
pub fn writeWord(self: *Self, addr: u32, n: u32) void {
    self.write(addr, u32, n);
}

fn write(self: *Self, addr: u32, comptime T: type, n: T) void {
    _ = self;
    _ = addr;
    _ = n;
    @panic("Unimplemented write");
}

test "static analysis" {
    std.testing.refAllDecls(@This());
}
