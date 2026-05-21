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

test "multiple literals" {
    const pattern = "abc";
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const tokens = try tokenizer.tokenize(allocator, pattern);

    var parser = RegexToAstParser.init(allocator, tokens);
    const ast_root = try parser.parse();

    try std.testing.expect(ast_root.* == .concat);
    try std.testing.expect(ast_root.concat.left.* == .concat);
    try std.testing.expectEqualDeep(RegexAstNode{ .literal = 'a' }, ast_root.concat.left.concat.left.*);
    try std.testing.expectEqualDeep(RegexAstNode{ .literal = 'b' }, ast_root.concat.left.concat.right.*);
    try std.testing.expectEqualDeep(RegexAstNode{ .literal = 'c' }, ast_root.concat.right.*);
}
