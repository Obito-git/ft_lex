pub const RegexAstNode = union(enum) {
    literal: u8,
    concat: Binary,
    alternate: Binary,
    zero_or_more: *RegexAstNode, // *
    one_or_more: *RegexAstNode, // +
    zero_or_one: *RegexAstNode, // ?
    dot,
    bracketExpression: BracketExpression,
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
