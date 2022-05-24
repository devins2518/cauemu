const std = @import("std");
const sdl = @cImport({
    @cInclude("SDL.h");
});
const utils = @import("utils.zig");
const Bus = @import("Bus.zig");
const Self = @This();
const Height = 160;
const Width = 240;

const LCDCAddr = 0x04000000;
const GreenSwapAddr = 0x04000002;
const LCDSAddr = 0x04000004;
const VCounterAddr = 0x04000006;
const Bg0ControlAddr = 0x04000008;
const Bg1ControlAddr = 0x0400000A;
const Bg2ControlAddr = 0x0400000C;
const Bg3ControlAddr = 0x0400000E;
const BgScrollAddr = 0x04000010;

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
    bg_mode: enum(u3) {
        bg0 = 0,
        bg1 = 1,
        bg2 = 2,
        bg3 = 3,
        bg4 = 4,
        bg5 = 5,
        _prohib0 = 6,
        _prohib1 = 7,
    },
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
bg_scroll: *packed struct {
    bg0_x: u8,
    _: u8,
    bg0_y: u8,
    __: u8,
    bg1_x: u8,
    ___: u8,
    bg1_y: u8,
    ____: u8,
    bg2_x: u8,
    _____: u8,
    bg2_y: u8,
    ______: u8,
    bg3_x: u8,
    _______: u8,
    bg3_y: u8,
    ________: u8,
},

pub fn init(bus: *Bus) Self {
    var window: ?*sdl.SDL_Window = undefined;
    var renderer: ?*sdl.SDL_Renderer = undefined;
    if (sdl.SDL_CreateWindowAndRenderer(Width, Height, 0, &window, &renderer) != 0)
        utils.sdlPanic();
    return .{
        .window = window.?,
        .renderer = renderer.?,
        .lcdc = @ptrCast(utils.Field(Self, .lcdc), bus.getAddr(LCDCAddr)),
        .green_swap = @ptrCast(utils.Field(Self, .green_swap), bus.getAddr(GreenSwapAddr)),
        .lcds = @ptrCast(utils.Field(Self, .lcds), bus.getAddr(LCDCAddr)),
        .vcounter = @ptrCast(utils.Field(Self, .vcounter), bus.getAddr(VCounterAddr)),
        .bg0_control = @ptrCast(*BGControl, bus.getAddr(Bg0ControlAddr)),
        .bg1_control = @ptrCast(*BGControl, bus.getAddr(Bg1ControlAddr)),
        .bg2_control = @ptrCast(*BGControl, bus.getAddr(Bg2ControlAddr)),
        .bg3_control = @ptrCast(*BGControl, bus.getAddr(Bg3ControlAddr)),
        .bg_scroll = @ptrCast(utils.Field(Self, .bg_scroll), bus.getAddr(BgScrollAddr)),
    };
}

pub fn deinit(self: Self) void {
    _ = self;
}

test "static analysis" {
    std.testing.refAllDecls(@This());
}
