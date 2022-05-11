const std = @import("std");
const Self = @This();

const BIOS_START = 0x00000000;
const BIOS_END = 0x00003FFF;
const BIOS_SIZE = BIOS_END - BIOS_START + 1;
const WRAM_OB_START = 0x02000000;
const WRAM_OB_END = 0x0203FFFF;
const WRAM_OB_SIZE = WRAM_OB_END - WRAM_OB_START + 1;
const WRAM_OC_START = 0x03000000;
const WRAM_OC_END = 0x03007FFF;
const WRAM_OC_SIZE = WRAM_OC_END - WRAM_OC_START + 1;
const IO_START = 0x04000000;
const IO_END = 0x040003FE;
const IO_SIZE = IO_END - IO_START + 1;

bios: [BIOS_SIZE]u8,
wram_ob: [WRAM_OB_SIZE]u8,
wram_oc: [WRAM_OC_SIZE]u8,
io: [IO_SIZE]u8,

pub fn init() Self {
    const bios = [_]u8{undefined} ** BIOS_SIZE;
    const wram_ob = [_]u8{undefined} ** WRAM_OB_SIZE;
    const wram_oc = [_]u8{undefined} ** WRAM_OC_SIZE;
    const io = [_]u8{undefined} ** IO_SIZE;
    return .{
        .bios = bios,
        .wram_ob = wram_ob,
        .wram_oc = wram_oc,
        .io = io,
    };
}

pub fn deinit(self: Self) void {
    _ = self;
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

test "memory sizes" {
    try std.testing.expect(BIOS_SIZE == 0x00004000);
    try std.testing.expect(WRAM_OB_SIZE == 0x000040000);
    try std.testing.expect(WRAM_OC_SIZE == 0x000008000);
    try std.testing.expect(IO_SIZE == 0x0000003FF);
}
