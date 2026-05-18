const std = @import("std");
const tokenizer = @import("tokenizer.zig");

pub const Token = tokenizer.Token;
pub const tokenize = tokenizer.tokenize;
pub const ast = @import("ast.zig");

pub const Regex = struct {
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, pattern: []const u8) !Regex {
        const tokens = try tokenizer.tokenize(allocator, pattern);
        defer allocator.free(tokens);

        return Regex{
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Regex) void {
        // TODO: placeholder for future cleanup logic
        _ = self;
    }
};
