const std = @import("std");
const sdl = @cImport({
    @cInclude("SDL.h");
});
const utils = @import("utils.zig");
const Bus = @import("Bus.zig");
const Self = @This();
const Height = 160;
const Width = 240;

const LCDCAddr = 0x4000000;
const GreenSwapAddr = 0x4000002;
const LCDStatusAddr = 0x4000004;
const VCounterAddr = 0x4000006;
const Bg0CntAddr = 0x4000008;
const Bg1CntAddr = 0x400000A;
const Bg2CntAddr = 0x400000C;
const Bg3CntAddr = 0x400000E;
const Bg0XOffsetAddr = 0x4000010;
const Bg0YOffsetAddr = 0x4000012;
const Bg1XOffsetAddr = 0x4000014;
const Bg1YOffsetAddr = 0x4000016;
const Bg2XOffsetAddr = 0x4000018;
const Bg2YOffsetAddr = 0x400001A;
const Bg3XOffsetAddr = 0x400001C;
const Bg3YOffsetAddr = 0x400001E;
const Bg2dxAddr = 0x4000020;
const Bg2dmxAddr = 0x4000022;
const Bg2dyAddr = 0x4000024;
const Bg2dmyAddr = 0x4000026;
const Bg2RefXAddr = 0x4000028;
const Bg2RefYAddr = 0x400002C;
const Bg3dxAddr = 0x4000030;
const Bg3dmxAddr = 0x4000032;
const Bg3dyAddr = 0x4000034;
const Bg3dmyAddr = 0x4000036;
const Bg3RefXAddr = 0x4000038;
const Bg3RefYAddr = 0x400003C;
const Win0XDimAddr = 0x4000040;
const Win1XDimAddr = 0x4000042;
const Win0YDimAddr = 0x4000044;
const Win1YDimAddr = 0x4000046;
const WinInAddr = 0x4000048;
const WinOutAddr = 0x400004A;
const MosaicAddr = 0x400004C;
const BldCntAddr = 0x4000050;
const BldAlphaAddr = 0x4000052;
const BldyAddr = 0x4000054;

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

const BGControl = packed struct {
    bg_priority: u2,
    char_base_block: u2,
    _: u2,
    mosaic: bool,
    color_palette: enum(u1) {
        _16_16 = 0b0,
        _256_1 = 0b1,
    },
    screen_base_block: u5,
    display_overflow: enum(u1) {
        transparent = 0b0,
        wraparound = 0b1,
    },
    screen_size: enum(u2) {
        map_256_256 = 0x0,
        map_512_256 = 0x1,
        map_256_512 = 0x2,
        map_512_512 = 0x3,
    },
};

const XOffset = packed struct {
    xoff: u8,
    _: u8,
};
const YOffset = packed struct {
    yoff: u8,
    _: u8,
};

const RefPoint = packed struct {
    _: u4,
    signed: bool,
    int: u19,
    frac: u8,
};

const RotScaleParam = packed struct {
    signed: bool,
    int: u7,
    frac: u8,
};

const WinDim = packed struct {
    x1: u8,
    x2: u8,
};

const BGMode = enum(u3) {
    bg0 = 0,
    bg1 = 1,
    bg2 = 2,
    bg3 = 3,
    bg4 = 4,
    bg5 = 5,
};

const OAMAttributes = packed struct {
    attr_2: packed struct {
        palette: u4,
        priority: u2,
        name: u10,
    },
    attr_1: packed struct {
        size: u3,
        attr: packed union {
            rot_scale: u5,
            flips: packed struct {
                _: u3,
                hflip: bool,
                vflip: bool,
            },
        },
        x_coord: u8,
    },
    attr_0: packed struct {
        obj_shape: enum(u2) {
            square = 0x0,
            horizontal = 0x1,
            vertical = 0x2,
        },
        palette: enum(u1) {
            _16_16 = 0x0,
            _256_1 = 0x1,
        },
        mosaic: bool,
        mode: enum(u2) {
            normal = 0x0,
            semitrans = 0x1,
            window = 0x2,
        },
        attr: packed union {
            double: bool,
            disable: bool,
        },
        rot_scale: bool,
        y_coord: u8,
    },
};

window: *sdl.SDL_Window,
renderer: *sdl.SDL_Renderer,
lcdc: *packed struct {
    display_window_obj: enum(u1) { off = 0b0, on = 0b1 },
    display_window1: enum(u1) { off = 0b0, on = 0b1 },
    display_window0: enum(u1) { off = 0b0, on = 0b1 },
    display_obj: enum(u1) { off = 0b0, on = 0b1 },
    display_bg3: enum(u1) { off = 0b0, on = 0b1 },
    display_bg2: enum(u1) { off = 0b0, on = 0b1 },
    display_bg1: enum(u1) { off = 0b0, on = 0b1 },
    display_bg0: enum(u1) { off = 0b0, on = 0b1 },
    forced_blank: enum(u1) { no = 0b0, yes = 0b1 }, // Allow access to VRAM, palette, OAM
    obj_char_mapping: enum(u1) { two_dimen = 0b0, three_dimen = 0b1 },
    hblank_oam_access: enum(u1) { no = 0b0, yes = 0b1 },
    display_frame: enum(u1) { front = 0b0, back = 0b1 }, // TODO: ??
    cgb_mode: enum(u1) { gba = 0b0, cgb = 0b1 },
    bg_mode: BGMode,
},
green_swap: *packed struct {
    _: u15,
    swap: enum(u1) {
        normal = 0b0,
        swap = 0b1,
    },
},
lcds: *packed struct {
    vcounter_setting: u8,
    _: u2,
    vcounter_irq: bool,
    hblank_irq: bool,
    vblank_irq: bool,
    vcounter: bool,
    hblank: bool,
    vblank: bool,
},
vcounter: *packed struct {
    ly: u8,
    _: u8,
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
    _: u2,
    win1_color_effect: bool,
    win1_obj_enable: bool,
    win1_bg3_enable: bool,
    win1_bg2_enable: bool,
    win1_bg1_enable: bool,
    win1_bg0_enable: bool,
    __: u2,
    win0_color_effect: bool,
    win0_obj_enable: bool,
    win0_bg3_enable: bool,
    win0_bg2_enable: bool,
    win0_bg1_enable: bool,
    win0_bg0_enable: bool,
},
winout: *packed struct {
    _: u2,
    obj_color_effect: bool,
    obj_obj_enable: bool,
    obj_bg3_enable: bool,
    obj_bg2_enable: bool,
    obj_bg1_enable: bool,
    obj_bg0_enable: bool,
    __: u2,
    outside_color_effect: bool,
    outside_obj_enable: bool,
    outside_bg3_enable: bool,
    outside_bg2_enable: bool,
    outside_bg1_enable: bool,
    outside_bg0_enable: bool,
},
mosaic_size: *packed struct {
    _: u16,
    obj_mosaic_y_size: u4,
    obj_mosaic_x_size: u4,
    bg_mosaic_y_size: u4,
    bg_mosaic_x_size: u4,
},
blend_control: *packed struct {
    _: u2,
    bd_target_pixel2: u1,
    obj_target_pixel2: u1,
    bg3_target_pixel2: u1,
    bg2_target_pixel2: u1,
    bg1_target_pixel2: u1,
    bg0_target_pixel2: u1,
    color_effect: enum(u2) {
        none = 0x0,
        alpha_blend = 0x1,
        brighten = 0x2,
        darken = 0x3,
    },
    bd_target_pixel1: u1,
    obj_target_pixel1: u1,
    bg3_target_pixel1: u1,
    bg2_target_pixel1: u1,
    bg1_target_pixel1: u1,
    bg0_target_pixel1: u1,
},
blend_alpha: *packed struct {
    _: u3,
    evb_coeff: u5,
    __: u3,
    eva_coeff: u5,
},
brightness: *packed struct {
    _: u27,
    evy_coeff: u5,
},
clocks: usize = 0,

pub fn init(alloc: *std.mem.Allocator, bus: *Bus) !*Self {
    var window: ?*sdl.SDL_Window = undefined;
    var renderer: ?*sdl.SDL_Renderer = undefined;
    if (sdl.SDL_CreateWindowAndRenderer(Width, Height, 0, &window, &renderer) != 0)
        utils.sdlPanic();
    const self = try alloc.create(Self);
    self.* = .{
        .window = window.?,
        .renderer = renderer.?,
        .lcdc = @ptrCast(utils.Field(Self, .lcdc), bus.getAddr(LCDCAddr, Bus.halfWordAlign)),
        .green_swap = @ptrCast(utils.Field(Self, .green_swap), bus.getAddr(GreenSwapAddr, Bus.halfWordAlign)),
        .lcds = @ptrCast(utils.Field(Self, .lcds), bus.getAddr(LCDCAddr, Bus.halfWordAlign)),
        .vcounter = @ptrCast(utils.Field(Self, .vcounter), bus.getAddr(VCounterAddr, Bus.halfWordAlign)),
        .bg0_control = @ptrCast(*BGControl, bus.getAddr(Bg0CntAddr, Bus.halfWordAlign)),
        .bg1_control = @ptrCast(*BGControl, bus.getAddr(Bg1CntAddr, Bus.halfWordAlign)),
        .bg2_control = @ptrCast(*BGControl, bus.getAddr(Bg2CntAddr, Bus.halfWordAlign)),
        .bg3_control = @ptrCast(*BGControl, bus.getAddr(Bg3CntAddr, Bus.halfWordAlign)),
        .bg0_x = @ptrCast(*XOffset, bus.getAddr(Bg0XOffsetAddr, Bus.halfWordAlign)),
        .bg0_y = @ptrCast(*YOffset, bus.getAddr(Bg0YOffsetAddr, Bus.halfWordAlign)),
        .bg1_x = @ptrCast(*XOffset, bus.getAddr(Bg1XOffsetAddr, Bus.halfWordAlign)),
        .bg1_y = @ptrCast(*YOffset, bus.getAddr(Bg1YOffsetAddr, Bus.halfWordAlign)),
        .bg2_x = @ptrCast(*XOffset, bus.getAddr(Bg2XOffsetAddr, Bus.halfWordAlign)),
        .bg2_y = @ptrCast(*YOffset, bus.getAddr(Bg2YOffsetAddr, Bus.halfWordAlign)),
        .bg3_x = @ptrCast(*XOffset, bus.getAddr(Bg3XOffsetAddr, Bus.halfWordAlign)),
        .bg3_y = @ptrCast(*YOffset, bus.getAddr(Bg3YOffsetAddr, Bus.halfWordAlign)),
        .bg2_refx = @ptrCast(*RefPoint, bus.getAddr(Bg2RefXAddr, Bus.wordAlign)),
        .bg2_refy = @ptrCast(*RefPoint, bus.getAddr(Bg2RefYAddr, Bus.wordAlign)),
        .bg3_refx = @ptrCast(*RefPoint, bus.getAddr(Bg3RefXAddr, Bus.wordAlign)),
        .bg3_refy = @ptrCast(*RefPoint, bus.getAddr(Bg3RefYAddr, Bus.wordAlign)),
        .bg2_dx = @ptrCast(*RotScaleParam, bus.getAddr(Bg2dxAddr, Bus.halfWordAlign)),
        .bg2_dmx = @ptrCast(*RotScaleParam, bus.getAddr(Bg2dmxAddr, Bus.halfWordAlign)),
        .bg2_dy = @ptrCast(*RotScaleParam, bus.getAddr(Bg2dyAddr, Bus.halfWordAlign)),
        .bg2_dmy = @ptrCast(*RotScaleParam, bus.getAddr(Bg2dmyAddr, Bus.halfWordAlign)),
        .bg3_dx = @ptrCast(*RotScaleParam, bus.getAddr(Bg3dxAddr, Bus.halfWordAlign)),
        .bg3_dmx = @ptrCast(*RotScaleParam, bus.getAddr(Bg3dmxAddr, Bus.halfWordAlign)),
        .bg3_dy = @ptrCast(*RotScaleParam, bus.getAddr(Bg3dyAddr, Bus.halfWordAlign)),
        .bg3_dmy = @ptrCast(*RotScaleParam, bus.getAddr(Bg3dmyAddr, Bus.halfWordAlign)),
        .win0_xdim = @ptrCast(*WinDim, bus.getAddr(Win0XDimAddr, Bus.halfWordAlign)),
        .win0_ydim = @ptrCast(*WinDim, bus.getAddr(Win0YDimAddr, Bus.halfWordAlign)),
        .win1_xdim = @ptrCast(*WinDim, bus.getAddr(Win1XDimAddr, Bus.halfWordAlign)),
        .win1_ydim = @ptrCast(*WinDim, bus.getAddr(Win1YDimAddr, Bus.halfWordAlign)),
        .winin = @ptrCast(utils.Field(Self, .winin), bus.getAddr(WinInAddr, Bus.halfWordAlign)),
        .winout = @ptrCast(utils.Field(Self, .winout), bus.getAddr(WinOutAddr, Bus.halfWordAlign)),
        .mosaic_size = @ptrCast(utils.Field(Self, .mosaic_size), bus.getAddr(MosaicAddr, Bus.wordAlign)),
        .blend_control = @ptrCast(utils.Field(Self, .blend_control), bus.getAddr(BldCntAddr, Bus.halfWordAlign)),
        .blend_alpha = @ptrCast(utils.Field(Self, .blend_alpha), bus.getAddr(BldAlphaAddr, Bus.halfWordAlign)),
        .brightness = @ptrCast(utils.Field(Self, .brightness), bus.getAddr(BldyAddr, Bus.wordAlign)),
    };
    return self;
}

pub fn clock(self: *Self) void {
    self.clocks += 1;
    if (self.clocks % 4 == 0) {
        // Do stuff
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
