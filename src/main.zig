const std = @import("std");
const Io = std.Io;

const lvm_zig = @import("lvm_zig");

pub fn main(init: std.process.Init) !void {
    const arena: std.mem.Allocator = init.arena.allocator();

    // Accessing command line arguments:
    const args = try init.minimal.args.toSlice(arena);
    if (args.len != 2) {
        std.log.err("Should have exactly ", .{});
    }
    for (args) |arg| {
        std.log.info("arg: {s}", .{arg});
    }
}
