const std = @import("std");
const tokenizer = @import("tokenizer.zig");

pub const Token = tokenizer.Token;
pub const tokenize = tokenizer.tokenize;
pub const ast = @import("ast.zig");

pub const Regex = struct {
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, pattern: []const u8) !Regex {
        var arena_tmp = std.heap.ArenaAllocator.init(allocator);
        defer arena_tmp.deinit();
        const scratch_allocator = arena_tmp.allocator();

        const tokens = try tokenizer.tokenize(scratch_allocator, pattern);
        var parser = ast.RegexToAstParser.init(scratch_allocator, tokens);
        const ast_root = try parser.parse();

        _ = ast_root; // TODO

        return Regex{
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Regex) void {
        // TODO: placeholder for future cleanup logic
        _ = self;
    }
};

test {
    _ = tokenizer;
    _ = ast;
}
