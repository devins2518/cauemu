const std = @import("std");
const Pkg = std.build.Pkg;
const FileSource = std.build.FileSource;

pub const pkgs = struct {
    pub const matches = Pkg{
        .name = "matches",
        .source = FileSource{
            .path = ".gyro/matches-devins2518-0.1.0-astrolabe.pm/pkg/src/main.zig",
        },
    };

    pub fn addAllTo(artifact: *std.build.LibExeObjStep) void {
        artifact.addPackage(pkgs.matches);
    }
};

pub const exports = struct {
    pub const cauemu = Pkg{
        .name = "cauemu",
        .source = FileSource{ .path = "src/main.zig" },
        .dependencies = &[_]Pkg{
            pkgs.matches,
        },
    };
};
