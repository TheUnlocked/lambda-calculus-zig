const std = @import("std");
const alloc_utils = @import("alloc_utils.zig");

pub const Symbol = u32;

pub const ElementTag = enum {
    variable,
    lambda,
    apply,
};

pub fn Element(mutable: bool) type {
    return union (ElementTag) {
        const Self = @This();
        const EltPtr = if (mutable) *Self else *const Self;

        variable: Symbol,
        lambda: struct { param: Symbol, body: EltPtr },
        apply: struct { target: EltPtr, arg: EltPtr },

        pub fn toString(self: Self, allocator: std.mem.Allocator) std.fmt.AllocPrintError![]const u8 {
            switch (self) {
                .variable => |sym| return try std.fmt.allocPrint(allocator, "@{d}", .{ sym }),
                .lambda => |lam| {
                    const bodystr = try lam.body.toString(allocator);
                    defer allocator.free(bodystr);

                    return try std.fmt.allocPrint(allocator, "(\\@{d} . {s})", .{ lam.param, bodystr });
                },
                .apply => |app| {
                    const targetstr = try app.target.toString(allocator);
                    defer allocator.free(targetstr);

                    const appstr = try app.arg.toString(allocator);
                    defer allocator.free(appstr);

                    return try std.fmt.allocPrint(allocator, "({s} {s})", .{ targetstr, appstr });
                },
            }
        }

        pub fn equals(self: Self, other: Self) bool {
            return switch (self) {
                .variable => |selfsym| switch (other) { .variable => |othersym| selfsym == othersym, else => false },
                .lambda => |selflam| switch (other) {
                    .lambda => |otherlam| selflam.param == otherlam.param and selflam.body.equals(otherlam.body.*),
                    else => false
                },
                .apply => |selfapp| switch (other) {
                    .apply => |otherapp| selfapp.target.equals(otherapp.target.*) and selfapp.arg.equals(otherapp.arg.*),
                    else => false
                },
            };
        }

        pub fn free(self: *const Self, allocator: std.mem.Allocator) void {
            var freeTable = std.AutoHashMap(*const Self, void).init(allocator);
            defer freeTable.deinit();
            self.freeRec(allocator, &freeTable);
        }

        fn freeRec(self: *const Self, allocator: std.mem.Allocator, freeSet: *std.AutoHashMap(*const Self, void)) void {
            if (freeSet.get(self) == null) {
                switch (self.*) {
                    .variable => {},
                    .lambda => |lam| {
                        lam.body.freeRec(allocator, freeSet);
                    },
                    .apply => |app| {
                        app.target.freeRec(allocator, freeSet);
                        app.arg.freeRec(allocator, freeSet);
                    },
                }
                // If this fails there's not much we can do to recover.
                allocator.destroy(self);
                freeSet.put(self, {}) catch unreachable;
            }
        }

        pub fn deepClone(self: *const Self, allocator: std.mem.Allocator, comptime toMutable: bool) std.mem.Allocator.Error!*Element(toMutable) {
            return try switch (self.*) {
                .variable => |sym| alloc_utils.createWith(allocator, Element(toMutable) {
                    .variable = sym
                }),
                .lambda => |lam| alloc_utils.createWith(allocator, Element(toMutable) {
                    .lambda = .{ .param = lam.param, .body = try lam.body.deepClone(allocator, toMutable) }
                }),
                .apply => |app| alloc_utils.createWith(allocator, Element(toMutable) {
                    .apply = .{ .target = try app.target.deepClone(allocator, toMutable), .arg = try app.arg.deepClone(allocator, toMutable) }
                }),
            };
        }

        pub fn castImmutable(self: *Element(true)) *Element(false) {
            return @ptrCast(*Element(false), self);
        }

        pub fn size(self: *Self) usize {
            return @sizeOf(Self) + switch (self.*) {
                .variable => 0,
                .lambda => |lam| lam.body.size(),
                .apply => |app| app.target.size() + app.arg.size(),
            };
        }
    };
}
