const std = @import("std");

const ClassFile = struct { minor_version: u16, major_version: u16 };

pub const regex = @import("regex/regex.zig");

test {
    _ = regex;
}
