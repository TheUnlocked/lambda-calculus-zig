const Element = @import("element.zig").Element(false);

pub const StatementTag = enum {
    define,
    loadFile,
    element,
};

pub const Statement = union (StatementTag) {
    element: *const Element,
    define: struct { name: []const u8, element: *const Element },
    loadFile: []const u8,
};