const std = @import("std");
const Self = @This();

const BIOS_FILE = @embedFile("../gba.bin");

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
const PAL_START = 0x05000000;
const PAL_END = 0x050003FF;
const PAL_SIZE = PAL_END - PAL_START + 1;
const VRAM_START = 0x06000000;
const VRAM_END = 0x06017FFF;
const VRAM_SIZE = VRAM_END - VRAM_START + 1;
const OAM_START = 0x07000000;
const OAM_END = 0x070003FF;
const OAM_SIZE = OAM_END - OAM_START + 1;

// TODO allocate to avoid ptr invalidation
bios: [BIOS_SIZE]u8,
wram_ob: [WRAM_OB_SIZE]u8,
wram_oc: [WRAM_OC_SIZE]u8,
io: [IO_SIZE]u8,
pal: [PAL_SIZE]u8,
vram: [VRAM_SIZE]u8,
oam: [OAM_SIZE]u8,

pub fn init() Self {
    var bios = [_]u8{undefined} ** BIOS_SIZE;
    @memcpy(&bios, BIOS_FILE, BIOS_SIZE);
    const wram_ob = [_]u8{undefined} ** WRAM_OB_SIZE;
    const wram_oc = [_]u8{undefined} ** WRAM_OC_SIZE;
    const io = [_]u8{undefined} ** IO_SIZE;
    const pal = [_]u8{undefined} ** PAL_SIZE;
    const vram = [_]u8{undefined} ** VRAM_SIZE;
    const oam = [_]u8{undefined} ** OAM_SIZE;
    return .{
        .bios = bios,
        .wram_ob = wram_ob,
        .wram_oc = wram_oc,
        .io = io,
        .pal = pal,
        .vram = vram,
        .oam = oam,
    };
}

pub fn deinit(self: Self) void {
    _ = self;
}

pub fn readByte(self: *Self, addr: u32) u8 {
    return self.getAddr(addr).*;
}
pub fn readHalfWord(self: *Self, addr: u32) u16 {
    return @intCast(u16, self.getAddr(addr + 1).*) << 8 |
        @intCast(u16, self.getAddr(addr).*);
}
pub fn readWord(self: *Self, addr: u32) u32 {
    return @intCast(u32, self.getAddr(addr + 3).*) << 24 |
        @intCast(u32, self.getAddr(addr + 2).*) << 16 |
        @intCast(u32, self.getAddr(addr + 1).*) << 8 |
        @intCast(u32, self.getAddr(addr).*);
}

pub fn writeByte(self: *Self, addr: u32, n: u8) void {
    self.getAddr(addr).* = n;
}
pub fn writeHalfWord(self: *Self, addr: u32, n: u16) void {
    @ptrCast(*u16, @alignCast(@alignOf(*u16), self.getAddr(addr))).* = n;
}
pub fn writeWord(self: *Self, addr: u32, n: u32) void {
    @ptrCast(*u32, @alignCast(@alignOf(*u32), self.getAddr(addr))).* = n;
}

pub fn getAddr(self: *Self, addr: u32) *u8 {
    return switch (addr) {
        0x00000000...0x00003FFF => &self.bios[addr],
        0x02000000...0x0203FFFF => &self.wram_ob[@mod(addr, WRAM_OB_START)],
        0x03000000...0x03007FFF => &self.wram_oc[@mod(addr, WRAM_OC_START)],
        0x04000000...0x040003FE => &self.io[@mod(addr, IO_START)],
        0x05000000...0x050003FF => &self.pal[@mod(addr, PAL_START)],
        0x06000000...0x06017FFF => &self.vram[@mod(addr, VRAM_START)],
        0x07000000...0x070003FF => &self.oam[@mod(addr, OAM_START)],
        else => std.debug.todo("Attempted to access unused memory region."),
    };
}

test "static analysis" {
    std.testing.refAllDecls(@This());
}

test "memory sizes" {
    try std.testing.expect(BIOS_SIZE == 0x00004000);
    try std.testing.expect(WRAM_OB_SIZE == 0x000040000);
    try std.testing.expect(WRAM_OC_SIZE == 0x000008000);
    try std.testing.expect(IO_SIZE == 0x0000003FF);
    try std.testing.expect(PAL_SIZE == 0x000000400);
    try std.testing.expect(VRAM_SIZE == 0x000018000);
    try std.testing.expect(OAM_SIZE == 0x000000400);
}
