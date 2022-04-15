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
            var applicativePart = x;

            if (n > 0) {
                var f = try alloc_utils.createWith(allocator, MutableElement { .variable = 0 });
                var i: u32 = 0;
                while (i < n) : (i += 1) {
                    applicativePart = try alloc_utils.createWith(allocator, MutableElement { .apply = .{
                        .target = f,
                        .arg = applicativePart,
                    } });
                }
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
const expect = std.testing.expect;

test "Church 0 is \\@0 . \\@1 . \\@1" {
    var n = try makeNumber(std.testing.allocator, 0, .church);
    defer n.free(std.testing.allocator);

    switch (n.*) {
        .lambda => |lam1| {
            try expect(lam1.param == 0);
            switch (lam1.body.*) {
                .lambda => |lam2| {
                    try expect(lam2.param == 1);
                    switch (lam2.body.*) {
                        .variable => |sym| try expect(sym == 1),
                        else => return error.Fail,
                    }
                },
                else => return error.Fail,
            }
        },
        else => return error.Fail,
    }
}

test "Numbers are emitted in alpha normal form" {
    var n1 = try makeNumber(std.testing.allocator, 30, .church);
    defer n1.free(std.testing.allocator);
    var n2 = try makeNumber(std.testing.allocator, 30, .church);
    defer n2.free(std.testing.allocator);
    try simplify.alphaNormalizeMut(std.testing.allocator, n2);
    try expect(n1.equals(n2.*));
}