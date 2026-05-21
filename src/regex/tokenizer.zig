const std = @import("std");

pub const TokenizationError = error{
    EscapedNothing,
};

pub const Token = union(enum) {
    literal: u8,
    dot,
    star,
    plus,
    question_mark,
    pipe,
    open_paren,
    close_paren,
    open_bracket,
    close_bracket,
    caret,
    colon,
    dash,

    pub fn is_quantifier(self: Token) bool {
        return switch (self) {
            // TODO: add {m,n} quantifiers
            .star, .plus, .question_mark => true,
            else => false,
        };
    }

    pub fn is_literal_or_group(self: Token) bool {
        return switch (self) {
            .literal, .dot, .open_paren, .open_bracket, .colon, .dash, .close_bracket => true,
            else => false,
        };
    }

    pub fn as_u8(self: Token) u8 {
        return switch (self) {
            .literal => self.literal,
            .dot => '.',
            .star => '*',
            .plus => '+',
            .question_mark => '?',
            .pipe => '|',
            .open_paren => '(',
            .close_paren => ')',
            .open_bracket => '[',
            .close_bracket => ']',
            .caret => '^',
            .colon => ':',
            .dash => '-',
        };
    }
};

pub fn tokenize(allocator: std.mem.Allocator, pattern: []const u8) ![]Token {
    var tokens: std.ArrayList(Token) = .empty;
    errdefer tokens.deinit(allocator);

    var i: usize = 0;
    while (i < pattern.len) : (i += 1) {
        const c = pattern[i];
        if (c == '\\') {
            if (i + 1 >= pattern.len) {
                return TokenizationError.EscapedNothing;
            }

            i += 1;
            try tokens.append(allocator, .{ .literal = decode_escaped_byte(pattern[i]) });
            continue;
        }

        try tokens.append(allocator, switch (c) {
            '.' => .dot,
            '*' => .star,
            '+' => .plus,
            '?' => .question_mark,
            '|' => .pipe,
            '(' => .open_paren,
            ')' => .close_paren,
            '[' => .open_bracket,
            ']' => .close_bracket,
            '^' => .caret,
            ':' => .colon,
            '-' => .dash,
            else => .{ .literal = c },
        });
    }

    return tokens.toOwnedSlice(allocator);
}

fn decode_escaped_byte(c: u8) u8 {
    return switch (c) {
        'a' => 0x07,
        'b' => 0x08,
        'f' => 0x0c,
        'n' => '\n',
        'r' => '\r',
        't' => '\t',
        'v' => 0x0b,
        else => c,
    };
}

test "tokenize operators and literals" {
    const testing = std.testing;
    const pattern = "a|b*";

    const tokens = try tokenize(testing.allocator, pattern);
    defer testing.allocator.free(tokens);

    const expected = [_]Token{
        .{ .literal = 'a' },
        .pipe,
        .{ .literal = 'b' },
        .star,
    };

    try expect_tokens(&expected, tokens);
}

test "tokenize escapes as literals" {
    const testing = std.testing;
    const pattern = "\\*\\(\\n";

    const tokens = try tokenize(testing.allocator, pattern);
    defer testing.allocator.free(tokens);

    const expected = [_]Token{
        .{ .literal = '*' },
        .{ .literal = '(' },
        .{ .literal = '\n' },
    };

    try expect_tokens(&expected, tokens);
}

test "tokenize trailing backslash errors" {
    const testing = std.testing;
    const pattern = "\\";

    try testing.expectError(TokenizationError.EscapedNothing, tokenize(testing.allocator, pattern));
}

fn expect_tokens(expected: []const Token, actual: []const Token) !void {
    try std.testing.expectEqual(expected.len, actual.len);

    for (expected, actual) |expected_token, actual_token| {
        try std.testing.expectEqualDeep(expected_token, actual_token);
    }
}
