const std = @import("std");
const utils = @import("utils.zig");
const alignedCreate = utils.alignedCreate;
const Allocator = std.mem.Allocator;
const Ppu = @import("Ppu.zig");
const Self = @This();

const BIOS_FILE = @embedFile("./gba.bin");

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

pub const wordAlign = @alignOf(u32);
pub const halfWordAlign = @alignOf(u16);
pub const byteAlign = @alignOf(u8);

bios: *align(wordAlign) [BIOS_SIZE]u8,
wram_ob: *align(wordAlign) [WRAM_OB_SIZE]u8,
wram_oc: *align(wordAlign) [WRAM_OC_SIZE]u8,
io: *align(wordAlign) [IO_SIZE]u8,
pal: *align(wordAlign) [PAL_SIZE]u8,
vram: *align(wordAlign) [VRAM_SIZE]u8,
oam: *align(wordAlign) [OAM_SIZE]u8,

ppu: *Ppu,

pub fn init(alloc: Allocator) !*Self {
    var bios = try alignedCreate(alloc, [BIOS_SIZE]u8, wordAlign);
    const wram_ob = try alignedCreate(alloc, [WRAM_OB_SIZE]u8, wordAlign);
    const wram_oc = try alignedCreate(alloc, [WRAM_OC_SIZE]u8, wordAlign);
    const io = try alignedCreate(alloc, [IO_SIZE]u8, wordAlign);
    const pal = try alignedCreate(alloc, [PAL_SIZE]u8, wordAlign);
    const vram = try alignedCreate(alloc, [VRAM_SIZE]u8, wordAlign);
    const oam = try alignedCreate(alloc, [OAM_SIZE]u8, wordAlign);

    @memcpy(bios, BIOS_FILE, BIOS_SIZE);

    var self = try alloc.create(Self);
    self.* = .{
        .bios = bios,
        .wram_ob = wram_ob,
        .wram_oc = wram_oc,
        .io = io,
        .pal = pal,
        .vram = vram,
        .oam = oam,
        .ppu = undefined,
    };
    return self;
}

pub fn registerPpu(self: *Self, ppu: *Ppu) void {
    self.ppu = ppu;
}

pub fn deinit(self: *Self, alloc: Allocator) void {
    alloc.destroy(self.bios);
    alloc.destroy(self.wram_ob);
    alloc.destroy(self.wram_oc);
    alloc.destroy(self.io);
    alloc.destroy(self.pal);
    alloc.destroy(self.vram);
    alloc.destroy(self.oam);
}

pub fn readByte(self: *Self, addr: u32) u8 {
    return self.getAddr(addr, byteAlign).*;
}
pub fn readHalfWord(self: *Self, addr: u32) u16 {
    const ptr = @ptrCast(*u16, self.getAddr(addr, halfWordAlign));
    return std.mem.readIntLittle(u16, @ptrCast(*[2]u8, ptr));
}
pub fn readWord(self: *Self, addr: u32) u32 {
    const ptr = @ptrCast(*u32, self.getAddr(addr, wordAlign));
    return std.mem.readIntLittle(u32, @ptrCast(*[4]u8, ptr));
}

pub fn writeByte(self: *Self, addr: u32, n: u8) void {
    self.getAddr(addr, byteAlign).* = n;
}
pub fn writeHalfWord(self: *Self, addr: u32, n: u16) void {
    const ptr = @ptrCast(*u16, self.getAddr(addr, halfWordAlign));
    std.mem.writeIntNative(u16, @ptrCast(*[2]u8, ptr), n);
}
pub fn writeWord(self: *Self, addr: u32, n: u32) void {
    const ptr = @ptrCast(*u32, self.getAddr(addr, wordAlign));
    std.mem.writeIntNative(u32, @ptrCast(*[4]u8, ptr), n);
}

pub fn getAddr(
    self: *Self,
    addr: u32,
    comptime alignment: ?u29,
) *align(alignment orelse byteAlign) u8 {
    const ptr = switch (addr) {
        0x00000000...0x00003FFF => &self.bios[addr],
        0x02000000...0x0203FFFF => &self.wram_ob[@mod(addr, WRAM_OB_START)],
        0x03000000...0x03007FFF => &self.wram_oc[@mod(addr, WRAM_OC_START)],
        0x04000000...0x040003FE => &self.io[@mod(addr, IO_START)],
        0x05000000...0x050003FF => if (self.ppu.lcds.hblank or
            self.ppu.lcds.vblank or self.ppu.lcdc.forced_blank)
        blk: {
            std.debug.print("here\n pal addr {}\n", .{@ptrToInt(self.pal)});
            break :blk &self.pal[@mod(addr, PAL_START)];
        } else @panic("Attempted to access PAL while locked."),
        0x06000000...0x06017FFF => if (self.ppu.lcds.hblank or
            self.ppu.lcds.vblank or self.ppu.lcdc.forced_blank)
            &self.vram[@mod(addr, VRAM_START)]
        else
            @panic("Attempted to access VRAM while locked."),
        0x07000000...0x070003FF => if (self.ppu.lcds.hblank or
            self.ppu.lcds.vblank or self.ppu.lcdc.forced_blank or self.ppu.lcdc.hblank_oam_access)
            &self.oam[@mod(addr, OAM_START)]
        else
            @panic("Attempted to access OAM while locked."),
        else => @panic("Attempted to access unused memory region."),
    };
    return @alignCast(alignment orelse byteAlign, ptr);
}

pub fn getAddrRaw(
    self: *Self,
    addr: u32,
    comptime alignment: ?u29,
) *align(alignment orelse byteAlign) u8 {
    const ptr = switch (addr) {
        0x00000000...0x00003FFF => &self.bios[addr],
        0x02000000...0x0203FFFF => &self.wram_ob[@mod(addr, WRAM_OB_START)],
        0x03000000...0x03007FFF => &self.wram_oc[@mod(addr, WRAM_OC_START)],
        0x04000000...0x040003FE => &self.io[@mod(addr, IO_START)],
        0x05000000...0x050003FF => &self.pal[@mod(addr, PAL_START)],
        0x06000000...0x06017FFF => &self.vram[@mod(addr, VRAM_START)],
        0x07000000...0x070003FF => &self.oam[@mod(addr, OAM_START)],
        else => @panic("Attempted to access unused memory region."),
    };
    return @alignCast(alignment orelse byteAlign, ptr);
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

test "using memory" {
    var alloc = std.heap.GeneralPurposeAllocator(.{}){};
    var bus = try Self.init(alloc.allocator());

    bus.writeWord(0x0, 0xCAFE0000);
    std.debug.assert(bus.readWord(0x0) == 0xCAFE0000);
    std.debug.assert(std.mem.eql(u8, bus.bios[0..4], &[_]u8{ 0x00, 0x00, 0xFE, 0xCA }));

    bus.writeHalfWord(0x4, 0xBEEF);
    std.debug.assert(bus.readHalfWord(0x4) == 0xBEEF);
    std.debug.assert(std.mem.eql(u8, bus.bios[4..6], &[_]u8{ 0xEF, 0xBE }));

    bus.writeByte(0x6, 0xAD);
    std.debug.assert(bus.readByte(0x6) == 0xAD);
    std.debug.assert(std.mem.eql(u8, bus.bios[6..7], &[_]u8{0xAD}));

    const ptr = @ptrCast(*u56, bus.getAddr(0x0, @alignOf(u56)));
    const adbeefcafe0000 = std.mem.readIntLittle(u56, @ptrCast(*[7]u8, ptr));
    std.debug.assert(adbeefcafe0000 == 0xADBEEFCAFE0000);
    std.debug.assert(std.mem.eql(u8, bus.bios[0..7], &[_]u8{ 0x00, 0x00, 0xFE, 0xCA, 0xEF, 0xBE, 0xAD }));
}
