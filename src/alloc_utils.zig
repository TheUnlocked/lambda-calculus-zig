const std = @import("std");
const Allocator = @import("std").mem.Allocator;

pub fn createWith(allocator: Allocator, obj: anytype) !*@TypeOf(obj) {
    const ptr = try allocator.create(@TypeOf(obj));
    ptr.* = obj;
    return ptr;
}