const std = @import("std");
const Symbol = @import("element.zig").Symbol;
const alloc_utils = @import("alloc_utils.zig");
const MutableElement = @import("element.zig").Element(true);

const NumberStyle = enum {
    church,
};

/// Creates a mutable element representing a number.
/// There are no guarantees as to the memory layout of the elements, or whether elements are reused.
/// Numbers are emitted in alpha normal form based on simplify.alphaNormalize
pub fn makeNumber(allocator: std.mem.Allocator, n: u32, comptime technique: NumberStyle) !*MutableElement {
    switch (technique) {
        .church => {
            var x = try alloc_utils.createWith(allocator, MutableElement { .variable = 1 });
            var f = try alloc_utils.createWith(allocator, MutableElement { .variable = 0 });
            var applicativePart = x;

            var i: u32 = 0;
            while (i < n) : (i += 1) {
                applicativePart = try alloc_utils.createWith(allocator, MutableElement { .apply = .{
                    .target = f,
                    .arg = applicativePart,
                } });
            }

            return try alloc_utils.createWith(allocator, MutableElement { .lambda = .{
                .param = 0,
                .body = try alloc_utils.createWith(allocator, MutableElement { .lambda = .{
                    .param = 1,
                    .body = applicativePart,
                } })
            } });
        }
    }
}

const simplify = @import("simplify.zig");

test "Numbers are emitted in alpha normal form" {
    var n1 = try makeNumber(std.testing.allocator, 30, .church);
    var n2 = try makeNumber(std.testing.allocator, 30, .church);
    try simplify.alphaNormalizeMut(std.testing.allocator, n2);
    try std.testing.expect(n1.equals(n2.*));
}