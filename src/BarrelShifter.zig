const std = @import("std");
const Self = @This();

const BarrelShifterLog = std.log.scoped(.BarrelShifter);

pub const ResultC = struct {
    imm: u32,
    carry: bool,
};

pub const ShiftType = enum(u3) {
    lsl = 0x0,
    lsr = 0x1,
    asr = 0x2,
    ror = 0x3,
    rrx = 0x4,
};

pub fn armExpandImmC(imm12: u12, carry_in: bool) ResultC {
    const unrotated_value = @as(u32, @truncate(u8, imm12));
    return shiftC(unrotated_value, .ror, 2 * @as(u6, @truncate(u4, imm12 >> 8)), carry_in);
}

pub fn armExpandImm(imm12: u12) u32 {
    return armExpandImmC(imm12, undefined).imm;
}

pub fn shiftC(value: u32, shift_ty: ShiftType, amt: u6, carry_in: bool) ResultC {
    std.debug.assert(!(shift_ty == .rrx and amt != 1));
    return if (amt == 0)
        ResultC{ .imm = value, .carry = carry_in }
    else switch (shift_ty) {
        .rrx => rrxC(value, carry_in),
        inline else => |ty| @field(Self, @tagName(ty) ++ "C")(value, amt),
    };
}

pub fn lslC(value: u32, amt: u6) ResultC {
    if (amt == 0)
        BarrelShifterLog.err("amt is 0!", .{});

    return if (amt >= 32) ResultC{
        .imm = 0,
        .carry = @truncate(u1, value) == 1,
    } else ResultC{
        .imm = value << @truncate(u5, amt),
        .carry = @truncate(u1, value >> @truncate(u5, 32 - amt)) == 1,
    };
}

pub fn lsrC(value: u32, amt: u6) ResultC {
    if (amt == 0)
        BarrelShifterLog.err("amt is 0!", .{});

    return if (amt >= 32) ResultC{
        .imm = 0,
        .carry = @truncate(u1, value >> 31) == 1,
    } else ResultC{
        .imm = value >> @truncate(u5, amt),
        .carry = @truncate(u1, value >> @truncate(u5, amt - 1)) == 1,
    };
}

pub fn asrC(value: u32, amt: u6) ResultC {
    if (amt == 0)
        BarrelShifterLog.err("amt is 0!", .{});

    return if (amt >= 32) ResultC{
        .imm = 0xFFFFFFFF,
        .carry = @truncate(u1, value >> 31) == 1,
    } else ResultC{
        .imm = @bitCast(u32, @bitCast(i32, value) >> @truncate(u5, amt)),
        .carry = @truncate(u1, value >> @truncate(u5, amt - 1)) == 1,
    };
}

pub fn rorC(value: u32, amt: u6) ResultC {
    if (amt == 0)
        BarrelShifterLog.err("amt is 0!", .{});

    const result = std.math.rotr(u32, value, amt);
    return ResultC{
        .imm = result,
        .carry = @truncate(u1, result >> 31) == 1,
    };
}

pub fn rrxC(value: u32, carry_in: bool) ResultC {
    return ResultC{
        .imm = @as(u32, @boolToInt(carry_in)) << 31 | value >> 1,
        .carry = @truncate(u1, value) == 1,
    };
}

test "static analysis" {
    std.testing.refAllDeclsRecursive(@This());
}

test "arithmetic shift" {
    try std.testing.expectEqual(
        ResultC{ .imm = 0xEAAAAAAA, .carry = true },
        asrC(0xAAAAAAAA, 2),
    );
}
