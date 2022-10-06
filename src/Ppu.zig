const std = @import("std");
const sdl = @cImport({
    @cInclude("SDL.h");
});
const utils = @import("utils.zig");
const Allocator = std.mem.Allocator;
const Bus = @import("Bus.zig");
const Self = @This();
const Height = 160;
const Width = 240;

const LCDCAddr = 0x04000000;
const GreenSwapAddr = 0x04000002;
const LCDStatusAddr = 0x04000004;
const VCounterAddr = 0x04000006;
const Bg0CntAddr = 0x04000008;
const Bg1CntAddr = 0x0400000A;
const Bg2CntAddr = 0x0400000C;
const Bg3CntAddr = 0x0400000E;
const Bg0XOffsetAddr = 0x04000010;
const Bg0YOffsetAddr = 0x04000012;
const Bg1XOffsetAddr = 0x04000014;
const Bg1YOffsetAddr = 0x04000016;
const Bg2XOffsetAddr = 0x04000018;
const Bg2YOffsetAddr = 0x0400001A;
const Bg3XOffsetAddr = 0x0400001C;
const Bg3YOffsetAddr = 0x0400001E;
const Bg2dxAddr = 0x04000020;
const Bg2dmxAddr = 0x04000022;
const Bg2dyAddr = 0x04000024;
const Bg2dmyAddr = 0x04000026;
const Bg2RefXAddr = 0x04000028;
const Bg2RefYAddr = 0x0400002C;
const Bg3dxAddr = 0x04000030;
const Bg3dmxAddr = 0x04000032;
const Bg3dyAddr = 0x04000034;
const Bg3dmyAddr = 0x04000036;
const Bg3RefXAddr = 0x04000038;
const Bg3RefYAddr = 0x0400003C;
const Win0XDimAddr = 0x04000040;
const Win1XDimAddr = 0x04000042;
const Win0YDimAddr = 0x04000044;
const Win1YDimAddr = 0x04000046;
const WinInAddr = 0x04000048;
const WinOutAddr = 0x0400004A;
const MosaicAddr = 0x0400004C;
const BldCntAddr = 0x04000050;
const BldAlphaAddr = 0x04000052;
const BldyAddr = 0x04000054;
const BgPaletteAddr = 0x05000000;
const ObjPaletteAddr = 0x05000200;

const HVisibleDots = Width;
const HVisibleCycles = HVisibleDots * 4;
const HHBlankDots = 68;
const HHBlankCycles = HHBlankDots * 4;
const HTotalDots = HVisibleDots + HHBlankDots;
const HTotalCycles = HTotalDots * 4;
const VVisibleLines = Height;
const VVisibleCycles = VVisibleLines * HTotalCycles;
const VHBlankLines = 68;
const VHBlankCycles = VHBlankLines * HTotalCycles;
const VTotalLines = VVisibleLines + VHBlankLines;
const VTotalCycles = VTotalLines * HTotalCycles;

const TILE_BG_START = 0x06000000;
const TILE_BG_END = 0x0600FFFF;
const TILE_BG_SIZE = TILE_BG_END - TILE_BG_START + 1;
const TILE_OBJ_START = 0x06010000;
const TILE_OBJ_END = 0x06017FFF;
const TILE_OBJ_SIZE = TILE_OBJ_END - TILE_OBJ_START + 1;
const BITMAP_IMG_BG_START = 0x06000000;
const BITMAP_IMG_BG_END = 0x06013FFF;
const BITMAP_IMG_BG_SIZE = BITMAP_IMG_BG_END - BITMAP_IMG_BG_START + 1;
const BITMAP_IMG_OBJ_START = 0x06014000;
const BITMAP_IMG_OBJ_END = 0x06017FFF;
const BITMAP_IMG_OBJ_SIZE = BITMAP_IMG_OBJ_END - BITMAP_IMG_OBJ_START + 1;
const BITMAP_FRAME0_START = 0x06000000;
const BITMAP_FRAME0_END = 0x06009FFF;
const BITMAP_FRAME0_SIZE = BITMAP_FRAME0_END - BITMAP_FRAME0_START + 1;
const BITMAP_FRAME1_START = 0x0600A000;
const BITMAP_FRAME1_END = 0x06013FFF;
const BITMAP_FRAME1_SIZE = BITMAP_FRAME1_END - BITMAP_FRAME1_START + 1;
const BITMAP_OBJ_START = 0x06014000;
const BITMAP_OBJ_END = 0x06017FFF;
const BITMAP_OBJ_SIZE = BITMAP_OBJ_END - BITMAP_OBJ_START + 1;

const Palette = extern struct {
    const Color = packed struct(u16) {
        _: u1,
        blue: u5,
        green: u5,
        red: u5,
    };
    palette: [256]Color,
};

const BGControl = packed struct {
    bg_priority: u2 = 0,
    char_base_block: u2 = 0,
    _: u2 = 0,
    mosaic: bool = false,
    color_palette: enum(u1) {
        _16_16 = 0b0,
        _256_1 = 0b1,
    } = ._16_16,
    screen_base_block: u5 = 0,
    display_overflow: enum(u1) {
        transparent = 0b0,
        wraparound = 0b1,
    } = .transparent,
    screen_size: enum(u2) {
        map_256_256 = 0x0,
        map_512_256 = 0x1,
        map_256_512 = 0x2,
        map_512_512 = 0x3,
    } = .map_256_256,
};

const XOffset = packed struct {
    xoff: u8 = 0,
    _: u8 = 0,
};
const YOffset = packed struct {
    yoff: u8 = 0,
    _: u8 = 0,
};

const RefPoint = packed struct {
    _: u4 = 0,
    signed: bool = false,
    int: u19 = 0,
    frac: u8 = 0,
};

const RotScaleParam = packed struct {
    signed: bool = false,
    int: u7 = 0,
    frac: u8 = 0,
};

const WinDim = packed struct {
    x1: u8 = 0,
    x2: u8 = 0,
};

const BGMode = enum(u3) {
    bg0 = 0,
    bg1 = 1,
    bg2 = 2,
    bg3 = 3,
    bg4 = 4,
    bg5 = 5,
};

const Depth = enum(u1) {
    half = 0x0,
    full = 0x1,
};

const OAMAttributes = packed struct {
    attr_2: packed struct {
        palette: u4 = 0,
        priority: u2 = 0,
        name: u10 = 0,
    } = .{},
    attr_1: packed struct {
        size: u3 = 0,
        attr: packed union {
            rot_scale: u5,
            flips: packed struct {
                _: u3,
                hflip: bool,
                vflip: bool,
            },
        },
        x_coord: u8 = 0,
    },
    attr_0: packed struct {
        obj_shape: enum(u2) {
            square = 0x0,
            horizontal = 0x1,
            vertical = 0x2,
        } = .square,
        palette: enum(u1) {
            _16_16 = 0x0,
            _256_1 = 0x1,
        } = ._16_16,
        mosaic: bool = false,
        mode: enum(u2) {
            normal = 0x0,
            semitrans = 0x1,
            window = 0x2,
        } = .normal,
        attr: packed union {
            double: bool,
            disable: bool,
        },
        rot_scale: bool = false,
        y_coord: u8 = 0,
    },
};

window: *sdl.SDL_Window,
renderer: *sdl.SDL_Renderer,
lcdc: *packed struct {
    display_window_obj: bool = false,
    display_window1: bool = false,
    display_window0: bool = false,
    display_obj: bool = false,
    display_bg3: bool = false,
    display_bg2: bool = false,
    display_bg1: bool = false,
    display_bg0: bool = false,
    forced_blank: bool = false, // Allow access to VRAM, palette, OAM
    obj_char_mapping: enum(u1) { two_dimen = 0b0, three_dimen = 0b1 } = .two_dimen,
    hblank_oam_access: bool = false,
    display_frame: enum(u1) { front = 0b0, back = 0b1 } = .front, // TODO: ??
    cgb_mode: enum(u1) { gba = 0b0, cgb = 0b1 } = .gba,
    bg_mode: BGMode = .bg0,
},
green_swap: *packed struct {
    _: u15 = 0,
    swap: enum(u1) {
        normal = 0b0,
        swap = 0b1,
    } = .normal,
},
lcds: *packed struct {
    vcounter_setting: u8 = 0,
    _: u2 = 0,
    vcounter_irq: bool = false,
    hblank_irq: bool = false,
    vblank_irq: bool = false,
    vcounter: bool = false,
    hblank: bool = true,
    vblank: bool = false,
},
vcounter: *packed struct {
    ly: u8 = 0,
    _: u8 = 0,
},
bg0_control: *BGControl,
bg1_control: *BGControl,
bg2_control: *BGControl,
bg3_control: *BGControl,
bg0_x: *XOffset,
bg0_y: *YOffset,
bg1_x: *XOffset,
bg1_y: *YOffset,
bg2_x: *XOffset,
bg2_y: *YOffset,
bg3_x: *XOffset,
bg3_y: *YOffset,
bg2_refx: *RefPoint,
bg2_refy: *RefPoint,
bg3_refx: *RefPoint,
bg3_refy: *RefPoint,
bg2_dx: *RotScaleParam,
bg2_dmx: *RotScaleParam,
bg2_dy: *RotScaleParam,
bg2_dmy: *RotScaleParam,
bg3_dx: *RotScaleParam,
bg3_dmx: *RotScaleParam,
bg3_dy: *RotScaleParam,
bg3_dmy: *RotScaleParam,
win0_xdim: *WinDim,
win0_ydim: *WinDim,
win1_xdim: *WinDim,
win1_ydim: *WinDim,
winin: *packed struct {
    _: u2 = 0,
    win1_color_effect: bool = false,
    win1_obj_enable: bool = false,
    win1_bg3_enable: bool = false,
    win1_bg2_enable: bool = false,
    win1_bg1_enable: bool = false,
    win1_bg0_enable: bool = false,
    __: u2 = 0,
    win0_color_effect: bool = false,
    win0_obj_enable: bool = false,
    win0_bg3_enable: bool = false,
    win0_bg2_enable: bool = false,
    win0_bg1_enable: bool = false,
    win0_bg0_enable: bool = false,
},
winout: *packed struct {
    _: u2 = 0,
    obj_color_effect: bool = false,
    obj_obj_enable: bool = false,
    obj_bg3_enable: bool = false,
    obj_bg2_enable: bool = false,
    obj_bg1_enable: bool = false,
    obj_bg0_enable: bool = false,
    __: u2 = 0,
    outside_color_effect: bool = false,
    outside_obj_enable: bool = false,
    outside_bg3_enable: bool = false,
    outside_bg2_enable: bool = false,
    outside_bg1_enable: bool = false,
    outside_bg0_enable: bool = false,
},
mosaic_size: *packed struct {
    _: u16 = 0,
    obj_mosaic_y_size: u4 = 0,
    obj_mosaic_x_size: u4 = 0,
    bg_mosaic_y_size: u4 = 0,
    bg_mosaic_x_size: u4 = 0,
},
blend_control: *packed struct {
    _: u2 = 0,
    bd_target_pixel2: u1 = 0,
    obj_target_pixel2: u1 = 0,
    bg3_target_pixel2: u1 = 0,
    bg2_target_pixel2: u1 = 0,
    bg1_target_pixel2: u1 = 0,
    bg0_target_pixel2: u1 = 0,
    color_effect: enum(u2) {
        none = 0x0,
        alpha_blend = 0x1,
        brighten = 0x2,
        darken = 0x3,
    } = .none,
    bd_target_pixel1: u1 = 0,
    obj_target_pixel1: u1 = 0,
    bg3_target_pixel1: u1 = 0,
    bg2_target_pixel1: u1 = 0,
    bg1_target_pixel1: u1 = 0,
    bg0_target_pixel1: u1 = 0,
},
blend_alpha: *packed struct {
    _: u3 = 0,
    evb_coeff: u5 = 0,
    __: u3 = 0,
    eva_coeff: u5 = 0,
},
brightness: *packed struct {
    _: u27 = 0,
    evy_coeff: u5 = 0,
},
bg_palette: *Palette,
obj_palette: *Palette,
bus: *Bus,
clocks: usize = 0,

pub fn init(alloc: Allocator, bus: *Bus) !*Self {
    var window: ?*sdl.SDL_Window = undefined;
    var renderer: ?*sdl.SDL_Renderer = undefined;
    if (sdl.SDL_CreateWindowAndRenderer(Width, Height, 0, &window, &renderer) != 0)
        utils.sdlPanic();
    const self = try alloc.create(Self);
    self.* = .{
        .window = window.?,
        .renderer = renderer.?,
        .lcdc = @ptrCast(utils.Field(Self, .lcdc), bus.getAddrRaw(LCDCAddr, Bus.halfWordAlign)),
        .green_swap = @ptrCast(utils.Field(Self, .green_swap), bus.getAddrRaw(GreenSwapAddr, Bus.halfWordAlign)),
        .lcds = @ptrCast(utils.Field(Self, .lcds), bus.getAddrRaw(LCDCAddr, Bus.halfWordAlign)),
        .vcounter = @ptrCast(utils.Field(Self, .vcounter), bus.getAddrRaw(VCounterAddr, Bus.halfWordAlign)),
        .bg0_control = @ptrCast(*BGControl, bus.getAddrRaw(Bg0CntAddr, Bus.halfWordAlign)),
        .bg1_control = @ptrCast(*BGControl, bus.getAddrRaw(Bg1CntAddr, Bus.halfWordAlign)),
        .bg2_control = @ptrCast(*BGControl, bus.getAddrRaw(Bg2CntAddr, Bus.halfWordAlign)),
        .bg3_control = @ptrCast(*BGControl, bus.getAddrRaw(Bg3CntAddr, Bus.halfWordAlign)),
        .bg0_x = @ptrCast(*XOffset, bus.getAddrRaw(Bg0XOffsetAddr, Bus.halfWordAlign)),
        .bg0_y = @ptrCast(*YOffset, bus.getAddrRaw(Bg0YOffsetAddr, Bus.halfWordAlign)),
        .bg1_x = @ptrCast(*XOffset, bus.getAddrRaw(Bg1XOffsetAddr, Bus.halfWordAlign)),
        .bg1_y = @ptrCast(*YOffset, bus.getAddrRaw(Bg1YOffsetAddr, Bus.halfWordAlign)),
        .bg2_x = @ptrCast(*XOffset, bus.getAddrRaw(Bg2XOffsetAddr, Bus.halfWordAlign)),
        .bg2_y = @ptrCast(*YOffset, bus.getAddrRaw(Bg2YOffsetAddr, Bus.halfWordAlign)),
        .bg3_x = @ptrCast(*XOffset, bus.getAddrRaw(Bg3XOffsetAddr, Bus.halfWordAlign)),
        .bg3_y = @ptrCast(*YOffset, bus.getAddrRaw(Bg3YOffsetAddr, Bus.halfWordAlign)),
        .bg2_refx = @ptrCast(*RefPoint, bus.getAddrRaw(Bg2RefXAddr, Bus.wordAlign)),
        .bg2_refy = @ptrCast(*RefPoint, bus.getAddrRaw(Bg2RefYAddr, Bus.wordAlign)),
        .bg3_refx = @ptrCast(*RefPoint, bus.getAddrRaw(Bg3RefXAddr, Bus.wordAlign)),
        .bg3_refy = @ptrCast(*RefPoint, bus.getAddrRaw(Bg3RefYAddr, Bus.wordAlign)),
        .bg2_dx = @ptrCast(*RotScaleParam, bus.getAddrRaw(Bg2dxAddr, Bus.halfWordAlign)),
        .bg2_dmx = @ptrCast(*RotScaleParam, bus.getAddrRaw(Bg2dmxAddr, Bus.halfWordAlign)),
        .bg2_dy = @ptrCast(*RotScaleParam, bus.getAddrRaw(Bg2dyAddr, Bus.halfWordAlign)),
        .bg2_dmy = @ptrCast(*RotScaleParam, bus.getAddrRaw(Bg2dmyAddr, Bus.halfWordAlign)),
        .bg3_dx = @ptrCast(*RotScaleParam, bus.getAddrRaw(Bg3dxAddr, Bus.halfWordAlign)),
        .bg3_dmx = @ptrCast(*RotScaleParam, bus.getAddrRaw(Bg3dmxAddr, Bus.halfWordAlign)),
        .bg3_dy = @ptrCast(*RotScaleParam, bus.getAddrRaw(Bg3dyAddr, Bus.halfWordAlign)),
        .bg3_dmy = @ptrCast(*RotScaleParam, bus.getAddrRaw(Bg3dmyAddr, Bus.halfWordAlign)),
        .win0_xdim = @ptrCast(*WinDim, bus.getAddrRaw(Win0XDimAddr, Bus.halfWordAlign)),
        .win0_ydim = @ptrCast(*WinDim, bus.getAddrRaw(Win0YDimAddr, Bus.halfWordAlign)),
        .win1_xdim = @ptrCast(*WinDim, bus.getAddrRaw(Win1XDimAddr, Bus.halfWordAlign)),
        .win1_ydim = @ptrCast(*WinDim, bus.getAddrRaw(Win1YDimAddr, Bus.halfWordAlign)),
        .winin = @ptrCast(utils.Field(Self, .winin), bus.getAddrRaw(WinInAddr, Bus.halfWordAlign)),
        .winout = @ptrCast(utils.Field(Self, .winout), bus.getAddrRaw(WinOutAddr, Bus.halfWordAlign)),
        .mosaic_size = @ptrCast(utils.Field(Self, .mosaic_size), bus.getAddrRaw(MosaicAddr, Bus.wordAlign)),
        .blend_control = @ptrCast(utils.Field(Self, .blend_control), bus.getAddrRaw(BldCntAddr, Bus.halfWordAlign)),
        .blend_alpha = @ptrCast(utils.Field(Self, .blend_alpha), bus.getAddrRaw(BldAlphaAddr, Bus.halfWordAlign)),
        .brightness = @ptrCast(utils.Field(Self, .brightness), bus.getAddrRaw(BldyAddr, Bus.wordAlign)),
        .bg_palette = @ptrCast(*Palette, bus.getAddrRaw(BgPaletteAddr, 2)),
        .obj_palette = @ptrCast(*Palette, bus.getAddrRaw(ObjPaletteAddr, 2)),
        .bus = bus,
    };
    return self;
}

pub fn reset(self: *Self) void {
    self.lcdc.* = .{};
    self.green_swap.* = .{};
    self.lcds.* = .{};
    self.vcounter.* = .{};
    self.bg0_control.* = .{};
    self.bg1_control.* = .{};
    self.bg2_control.* = .{};
    self.bg3_control.* = .{};
    self.bg0_x.* = .{};
    self.bg0_y.* = .{};
    self.bg1_x.* = .{};
    self.bg1_y.* = .{};
    self.bg2_x.* = .{};
    self.bg2_y.* = .{};
    self.bg3_x.* = .{};
    self.bg3_y.* = .{};
    self.bg2_refx.* = .{};
    self.bg2_refy.* = .{};
    self.bg3_refx.* = .{};
    self.bg3_refy.* = .{};
    self.bg2_dx.* = .{};
    self.bg2_dmx.* = .{};
    self.bg2_dy.* = .{};
    self.bg2_dmy.* = .{};
    self.bg3_dx.* = .{};
    self.bg3_dmx.* = .{};
    self.bg3_dy.* = .{};
    self.bg3_dmy.* = .{};
    self.win0_xdim.* = .{};
    self.win0_ydim.* = .{};
    self.win1_xdim.* = .{};
    self.win1_ydim.* = .{};
    self.winin.* = .{};
    self.winout.* = .{};
    self.mosaic_size.* = .{};
    self.blend_control.* = .{};
    self.blend_alpha.* = .{};
    self.brightness.* = .{};
}

pub fn clock(self: *Self) void {
    self.clocks += 1;
    if (self.clocks % 4 == 0) {
        if (self.lcdc.display_bg0)
            self.renderBackground(0);
        if (self.lcdc.display_bg1)
            self.renderBackground(1);
        if (self.lcdc.display_bg2)
            self.renderBackground(2);
        if (self.lcdc.display_bg3)
            self.renderBackground(3);
        // Do stuff
    }
}

fn renderBackground(self: *Self, comptime layer: comptime_int) void {
    switch (self.lcdc.bg_mode) {
        .bg0 => {
            const control = switch (layer) {
                0 => self.bg0_control,
                1 => self.bg1_control,
                2 => self.bg2_control,
                3 => self.bg3_control,
                else => unreachable,
            };
            const xoffset = switch (layer) {
                0 => self.bg0_x,
                1 => self.bg1_x,
                2 => self.bg2_x,
                3 => self.bg3_x,
                else => unreachable,
            };
            const yoffset = switch (layer) {
                0 => self.bg0_y,
                1 => self.bg1_y,
                2 => self.bg2_y,
                3 => self.bg3_y,
                else => unreachable,
            };
            const depth: Depth = switch (control.color_palette) {
                ._16_16 => .half,
                ._256_1 => .full,
            };
            const char_block = TILE_BG_START + (@intCast(u32, control.char_base_block) * 0x4000);
            const screen_block = TILE_BG_START + (@intCast(u32, control.screen_base_block) * 0x200);
            // TODO correct?
            if (screen_block >= char_block and screen_block <= char_block + 0x4000)
                std.log.warn("Screen block is within char block, rendering may be wrong.", .{});

            // Convert pixel height to tile number.
            const height: u32 = switch (control.screen_size) {
                .map_256_256, .map_512_256 => 256 / 8,
                .map_256_512, .map_512_512 => 512 / 8,
            };
            const width: u32 = switch (control.screen_size) {
                .map_256_256, .map_256_512 => 256 / 8,
                .map_512_256, .map_512_512 => 512 / 8,
            };

            var map: [512 / 8][512 / 8]u64 = undefined;

            self.constructMap(&map, height, width, depth, char_block, screen_block);

            _ = xoffset;
            _ = yoffset;
            std.debug.todo("renderBackground");
        },
        else => @panic("unimplemented"),
    }
}

fn constructMap(
    self: *Self,
    map: *[512 / 8][512 / 8]u64,
    height: u32,
    width: u32,
    depth: Depth,
    char_block: u32,
    screen_block: u32,
) void {
    const TextEntry = packed struct {
        palette_num: u4,
        vflip: bool,
        hflip: bool,
        tile_num: u10,
    };

    row: for (map) |*map_row, map_y_idx| {
        if (map_y_idx == height) break :row;

        cell: for (map_row) |*map_cell, map_x_idx| {
            if (map_x_idx == width) break :cell;

            const entry = @bitCast(TextEntry, self.bus.readHalfWord(screen_block +
                ((@truncate(u32, map_y_idx) + @truncate(u32, map_x_idx)) * 2)));
            _ = map_cell;
            // Construct tiles
            // TODO verify
            var tile: [8][8]u8 = undefined;
            for (tile) |*tile_row, tile_y_idx| {
                for (tile_row) |*pix, tile_x_idx| {
                    if (depth == .half) {
                        const byte = self.bus.readByte(char_block + (entry.tile_num / 2) +
                            @truncate(u32, tile_y_idx * 4) + @truncate(u32, tile_x_idx / 2));
                        if (@mod(tile_x_idx, 2) == 0) { // Left pixel
                            pix.* = byte & 0x0F;
                        } else { // Right pixel
                            pix.* = byte & 0xF0 >> 4;
                        }
                    } else {
                        const byte = self.bus.readByte(char_block + entry.tile_num +
                            @truncate(u32, tile_y_idx * 8) + @truncate(u32, tile_x_idx));
                        pix.* = byte;
                    }
                }
            }
        }
    }
}

pub fn deinit(self: Self) void {
    _ = self;
}

test "static analysis" {
    std.testing.refAllDecls(@This());
}

test "video timings" {
    std.debug.assert(HVisibleDots == 240);
    std.debug.assert(HHBlankDots == 68);
    std.debug.assert(HTotalDots == 308);
    std.debug.assert(HVisibleCycles == 960);
    std.debug.assert(HHBlankCycles == 272);
    std.debug.assert(HTotalCycles == 1232);
    std.debug.assert(VVisibleLines == 160);
    std.debug.assert(VHBlankLines == 68);
    std.debug.assert(VTotalLines == 228);
    std.debug.assert(VVisibleCycles == 197120);
    std.debug.assert(VHBlankCycles == 83776);
    std.debug.assert(VTotalCycles == 280896);
}

test "zig behavior" {
    std.debug.assert(0 / 2 == 0);
    std.debug.assert(1 / 2 == 0);
    std.debug.assert(2 / 2 == 1);
    std.debug.assert(3 / 2 == 1);
    std.debug.assert(4 / 2 == 2);
    std.debug.assert(5 / 2 == 2);
    std.debug.assert(6 / 2 == 3);
    std.debug.assert(7 / 2 == 3);
}
