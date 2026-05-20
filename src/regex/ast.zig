const std = @import("std");
const tokenizer = @import("tokenizer.zig");

pub const RegexParseError = error{
    UnexpectedEndOfTokens,
    UnexpectedToken,
    ExpressionStartedWithQuantifier,
    UnclosedParenthesis,
    PrecedentTokenIsNotQuantifiable,
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

pub fn parse(allocator: std.mem.Allocator, tokens: []const tokenizer.Token) !*RegexAstNode {
    const ggg = Parser.init(tokens).parse();
    _ = ggg;
}

const Parser = struct {
    tokens: []const tokenizer.Token,
    allocator: std.mem.Allocator,
    index: usize,

    fn init(allocator: std.mem.Allocator, tokens: []const tokenizer.Token) Parser {
        return Parser{
            .tokens = tokens,
            .allocator = allocator,
            .index = 0,
        };
    }

    fn is_index_in_bounds(self: *Parser) bool {
        return self.index < self.tokens.len;
    }

    fn peek(self: *Parser) ?tokenizer.Token {
        if (self.is_index_in_bounds()) {
            return self.tokens[self.index];
        }
        return null;
    }

    fn parse(self: *Parser) !*const RegexAstNode {
        return null;
    }

    fn parse_alternation(self: *Parser) !*const RegexAstNode {
        return null;
    }

    fn parse_concatenation(self: *Parser) !*const RegexAstNode {
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

    fn parse_quantifier(self: *Parser) !*const RegexAstNode {
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

    fn parse_literal_or_group(self: *Parser) !*const RegexAstNode {
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
            .open_square_bracket => {
                self.allocator.destroy(node);
                return try self.parse_square_bracket_expression();
            },
            .close_square_bracket => {
                node.* = .{ .literal = ']' };
                return node;
            },
            .open_paren => {
                if (self.peek() == .close_paren) {
                    self.index += 1; // consume the ')'
                    node.* = .epsilon;
                    return node;
                }
                const nested = try self.parse_alternation();
                if (self.peek() != .close_paren) {
                    return RegexParseError.UnclosedParenthesis;
                }
                self.index += 1; // consume the ')'
                self.allocator.destroy(node);
                return nested;
            },
            else => return RegexParseError.UnexpectedToken,
        }
    }

    fn map_quantifier(self: *Parser, node: *const RegexAstNode) !*const RegexAstNode {
        return null;
    }

    fn parse_square_bracket_expression(self: *Parser) !*const RegexAstNode {
        return null;
    }
};
