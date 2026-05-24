const std = @import("std");
const tokenizer = @import("tokenizer.zig");

pub const RegexParseError = error{
    UnexpectedEndOfTokens,
    UnexpectedToken,
    UnexpectedTokenInTheEndOfTheExpression,
    ExpressionStartedWithQuantifier,
    UnclosedParenthesis,
    PrecedentTokenIsNotQuantifiable,
    InvalidSquareBracketsExpressionRange,
};

pub const RegexAstNode = union(enum) {
    epsilon, // matches the empty string
    literal: u8,
    concat: Binary,
    alternate: Binary,
    zero_or_more: *RegexAstNode, // *
    one_or_more: *RegexAstNode, // +
    zero_or_one: *RegexAstNode, // ?
    dot, // any character
    bracket_expression: BracketExpression,

    fn is_quantifiable(self: RegexAstNode) bool {
        return switch (self) {
            .zero_or_more, .one_or_more, .zero_or_one => false,
            else => true,
        };
    }
};

pub const Binary = struct {
    left: *RegexAstNode,
    right: *RegexAstNode,
};

pub const BracketExpression = struct {
    inverted: bool,
    items: []const BracketItem,
};

pub const BracketItem = union(enum) {
    literal: u8,
    range: Range,
};

pub const Range = struct {
    start: u8,
    end: u8,
};

pub const RegexToAstParser = struct {
    const ParseError = std.mem.Allocator.Error || RegexParseError;

    tokens: []const tokenizer.Token,
    allocator: std.mem.Allocator,
    index: usize,

    pub fn init(allocator: std.mem.Allocator, tokens: []const tokenizer.Token) RegexToAstParser {
        return RegexToAstParser{
            .tokens = tokens,
            .allocator = allocator,
            .index = 0,
        };
    }

    fn is_index_in_bounds(self: *RegexToAstParser) bool {
        return self.index < self.tokens.len;
    }

    fn peek(self: *RegexToAstParser) ?tokenizer.Token {
        if (self.is_index_in_bounds()) {
            return self.tokens[self.index];
        }
        return null;
    }

    fn peek_second(self: *RegexToAstParser) ?tokenizer.Token {
        if (self.index + 1 < self.tokens.len) {
            return self.tokens[self.index + 1];
        }
        return null;
    }

    pub fn parse(self: *RegexToAstParser) ParseError!*RegexAstNode {
        if (!self.is_index_in_bounds()) {
            const node = try self.allocator.create(RegexAstNode);
            node.* = .epsilon;
            return node;
        }

        const root = try self.parse_alternation();

        if (self.is_index_in_bounds()) {
            return RegexParseError.UnexpectedTokenInTheEndOfTheExpression;
        }

        return root;
    }

    fn parse_alternation(self: *RegexToAstParser) ParseError!*RegexAstNode {
        var left = try self.parse_concatenation();

        while (self.peek()) |token| {
            if (token != .pipe) break;
            self.index += 1; // consume the '|'
            const right = try self.parse_concatenation();
            const new_left = try self.allocator.create(RegexAstNode);
            new_left.* = .{ .alternate = .{ .left = left, .right = right } };
            left = new_left;
        }
        return left;
    }

    fn parse_concatenation(self: *RegexToAstParser) ParseError!*RegexAstNode {
        var left = try self.parse_quantifier();

        while (true) {
            if (self.peek()) |x| {
                if (x.is_literal_or_group()) {
                    const right = try self.parse_quantifier();
                    const new_left = try self.allocator.create(RegexAstNode);
                    new_left.* = .{ .concat = .{ .left = left, .right = right } };
                    left = new_left;
                } else {
                    break;
                }
            } else {
                break;
            }
        }
        return left;
    }

    fn parse_quantifier(self: *RegexToAstParser) ParseError!*RegexAstNode {
        const node = try self.parse_literal_or_group();

        if (self.peek()) |x| {
            if (!x.is_quantifier()) {
                return node;
            }
            if (!node.is_quantifiable()) {
                return RegexParseError.PrecedentTokenIsNotQuantifiable;
            }
            return try self.map_quantifier(node);
        }

        return node;
    }

    fn parse_literal_or_group(self: *RegexToAstParser) ParseError!*RegexAstNode {
        if (!self.is_index_in_bounds()) {
            return RegexParseError.UnexpectedEndOfTokens;
        }

        if (self.tokens[self.index].is_quantifier()) {
            return RegexParseError.ExpressionStartedWithQuantifier;
        }

        const node = try self.allocator.create(RegexAstNode);
        const token = self.tokens[self.index];
        self.index += 1;

        switch (token) {
            .literal => {
                node.* = .{ .literal = token.literal };
                return node;
            },
            .colon => {
                node.* = .{ .literal = ':' };
                return node;
            },
            .dash => {
                node.* = .{ .literal = '-' };
                return node;
            },
            .dot => {
                node.* = .dot;
                return node;
            },
            .open_bracket => {
                return try self.parse_square_bracket_expression(node);
            },
            .close_bracket => {
                node.* = .{ .literal = ']' };
                return node;
            },
            .open_paren => {
                if (self.peek()) |next| {
                    if (next == .close_paren) {
                        self.index += 1; // consume the ')'
                        node.* = .epsilon;
                        return node;
                    }
                }

                const nested = try self.parse_alternation();
                if (self.peek()) |next| {
                    if (next != .close_paren) {
                        return RegexParseError.UnclosedParenthesis;
                    }
                    self.index += 1; // consume the ')'
                    self.allocator.destroy(node);
                    return nested;
                }

                return RegexParseError.UnclosedParenthesis;
            },
            else => return RegexParseError.UnexpectedToken,
        }
    }

    fn map_quantifier(self: *RegexToAstParser, node: *RegexAstNode) ParseError!*RegexAstNode {
        const quantifier = self.peek().?;
        self.index += 1; // consume the quantifier
        const new_node = try self.allocator.create(RegexAstNode);
        switch (quantifier) {
            .star => new_node.* = .{ .zero_or_more = node },
            .plus => new_node.* = .{ .one_or_more = node },
            .question_mark => new_node.* = .{ .zero_or_one = node },
            else => return RegexParseError.UnexpectedToken,
        }
        return new_node;
    }

    fn parse_square_bracket_expression(self: *RegexToAstParser, allocated_node: *RegexAstNode) ParseError!*RegexAstNode {
        var is_negated = false;

        if (self.peek()) |token| {
            if (token == .caret) {
                is_negated = true;
                self.index += 1; // consume the '^'
            }
        }

        //TODO: implement posix classes

        var items: std.ArrayList(BracketItem) = .empty;
        errdefer items.deinit(self.allocator);
        try items.append(self.allocator, try self.parse_square_bracket_range());

        while (self.peek()) |token| {
            if (token == .close_bracket) break;
            if (!self.is_index_in_bounds()) {
                return RegexParseError.UnexpectedEndOfTokens;
            }
            try items.append(self.allocator, try self.parse_square_bracket_range());
        }

        if (self.peek() == null) {
            return RegexParseError.UnexpectedEndOfTokens;
        }

        self.index += 1; // consume the ']'

        allocated_node.* = .{ .bracket_expression = .{
            .inverted = is_negated,
            .items = try items.toOwnedSlice(self.allocator),
        } };
        return allocated_node;
    }

    fn parse_square_bracket_range(self: *RegexToAstParser) ParseError!BracketItem {
        if (!self.is_index_in_bounds()) {
            return RegexParseError.UnexpectedEndOfTokens;
        }
        const left = self.tokens[self.index].as_u8();
        self.index += 1;

        const next = self.peek() orelse return .{ .literal = left };
        if (next != .dash) {
            return .{ .literal = left };
        }

        if (self.peek_second()) |after_dash| {
            if (after_dash == .close_bracket) {
                return .{ .literal = left };
            }
        } else {
            return .{ .literal = left };
        }

        self.index += 1; // consume the '-'

        if (!self.is_index_in_bounds()) {
            return RegexParseError.UnexpectedEndOfTokens;
        }
        const right = self.tokens[self.index].as_u8();
        self.index += 1;

        if (left > right) {
            return RegexParseError.InvalidSquareBracketsExpressionRange;
        }

        return .{ .range = .{ .start = left, .end = right } };
    }
};

test "parse precedence of alternation and concatenation" {
    const pattern = "ab|c";
    const expected =
        \\(alt
        \\  (concat
        \\    (literal 'a')
        \\    (literal 'b'))
        \\  (literal 'c'))
    ;

    try expect_ast_dump(pattern, expected);
}

test "parse errors" {
    const cases = [_]struct {
        pattern: []const u8,
        expected_error: RegexParseError,
    }{
        .{ .pattern = "*", .expected_error = error.ExpressionStartedWithQuantifier },
        .{ .pattern = "a)", .expected_error = error.UnexpectedTokenInTheEndOfTheExpression },
        .{ .pattern = "(a", .expected_error = error.UnclosedParenthesis },
        .{ .pattern = "[", .expected_error = error.UnexpectedEndOfTokens },
        .{ .pattern = "[]", .expected_error = error.UnexpectedEndOfTokens },
        .{ .pattern = "[z-a]", .expected_error = error.InvalidSquareBracketsExpressionRange },
    };

    for (cases) |case| {
        try expect_parse_error(case.pattern, case.expected_error);
    }
}

fn expect_ast_dump(pattern: []const u8, expected: []const u8) !void {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    const tokens = try tokenizer.tokenize(arena.allocator(), pattern);
    var parser = RegexToAstParser.init(arena.allocator(), tokens);
    const root = try parser.parse();

    const actual = try dump_ast(std.testing.allocator, root);
    defer std.testing.allocator.free(actual);

    try std.testing.expectEqualStrings(expected, actual);
}

fn expect_parse_error(pattern: []const u8, expected_error: RegexParseError) !void {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    const tokens = try tokenizer.tokenize(arena.allocator(), pattern);
    var parser = RegexToAstParser.init(arena.allocator(), tokens);

    try std.testing.expectError(expected_error, parser.parse());
}

fn dump_ast(allocator: std.mem.Allocator, root: *const RegexAstNode) ![]const u8 {
    var out: std.ArrayList(u8) = .empty;
    errdefer out.deinit(allocator);

    try write_node(allocator, &out, root, 0);
    return out.toOwnedSlice(allocator);
}

fn write_node(
    allocator: std.mem.Allocator,
    out: *std.ArrayList(u8),
    node: *const RegexAstNode,
    depth: usize,
) std.mem.Allocator.Error!void {
    switch (node.*) {
        .epsilon => try out.appendSlice(allocator, "(epsilon)"),
        .literal => |value| {
            try out.appendSlice(allocator, "(literal '");
            try append_escaped_char(allocator, out, value);
            try out.appendSlice(allocator, "')");
        },
        .dot => try out.appendSlice(allocator, "(dot)"),
        .concat => |binary| try write_binary_node(allocator, out, "concat", binary, depth),
        .alternate => |binary| try write_binary_node(allocator, out, "alt", binary, depth),
        .zero_or_more => |child| try write_unary_node(allocator, out, "zero_or_more", child, depth),
        .one_or_more => |child| try write_unary_node(allocator, out, "one_or_more", child, depth),
        .zero_or_one => |child| try write_unary_node(allocator, out, "zero_or_one", child, depth),
        .bracket_expression => try out.appendSlice(allocator, "(bracket_expression ...)"),
    }
}

fn write_binary_node(
    allocator: std.mem.Allocator,
    out: *std.ArrayList(u8),
    name: []const u8,
    binary: Binary,
    depth: usize,
) std.mem.Allocator.Error!void {
    try out.append(allocator, '(');
    try out.appendSlice(allocator, name);
    try out.append(allocator, '\n');
    try append_indent(allocator, out, depth + 1);
    try write_node(allocator, out, binary.left, depth + 1);
    try out.append(allocator, '\n');
    try append_indent(allocator, out, depth + 1);
    try write_node(allocator, out, binary.right, depth + 1);
    try out.append(allocator, ')');
}

fn write_unary_node(
    allocator: std.mem.Allocator,
    out: *std.ArrayList(u8),
    name: []const u8,
    child: *const RegexAstNode,
    depth: usize,
) std.mem.Allocator.Error!void {
    try out.append(allocator, '(');
    try out.appendSlice(allocator, name);
    try out.append(allocator, '\n');
    try append_indent(allocator, out, depth + 1);
    try write_node(allocator, out, child, depth + 1);
    try out.append(allocator, ')');
}

fn append_indent(allocator: std.mem.Allocator, out: *std.ArrayList(u8), depth: usize) std.mem.Allocator.Error!void {
    for (0..depth) |_| {
        try out.appendSlice(allocator, "  ");
    }
}

fn append_escaped_char(allocator: std.mem.Allocator, out: *std.ArrayList(u8), value: u8) std.mem.Allocator.Error!void {
    switch (value) {
        '\n' => try out.appendSlice(allocator, "\\n"),
        '\r' => try out.appendSlice(allocator, "\\r"),
        '\t' => try out.appendSlice(allocator, "\\t"),
        '\\' => try out.appendSlice(allocator, "\\\\"),
        '\'' => try out.appendSlice(allocator, "\\'"),
        else => try out.append(allocator, value),
    }
}
