const std = @import("std");
const createWith = @import("alloc_utils.zig").createWith;

pub fn free(allocator: std.mem.Allocator, ll: anytype) void {
    while (ll.popFirst()) |node| {
        allocator.destroy(node);
    }
}

pub fn clone(allocator: std.mem.Allocator, ll: anytype) !@TypeOf(ll) {
    if (ll.first) |first| {
        var root = createWith(allocator, first.*);
        var next = root;

        while (next.next) |nextNode| {
            next.next = createWith(allocator, nextNode.*);
            next = next.next;
        }

        return @TypeOf(ll) {
            .first = root,
        };
    }
    else {
        return ll;
    }
}