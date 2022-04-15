const std = @import("std");
const parser = @import("parser.zig");
const elt = @import("element.zig");
const simplify = @import("simplify.zig");

const Allocator = std.mem.Allocator;

pub fn main() anyerror!void {
    std.log.info("Enter an expression", .{});

    var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = general_purpose_allocator.allocator();
    
    const reader = std.io.getStdIn().reader();

    while (true) {
        const input = try readLine(allocator, reader);
        defer allocator.free(input);

        const statement = parser.parseStatement(allocator, input) catch |err| {
            const msg = try std.fmt.allocPrint(allocator, "Parse error: {s}", .{ @errorName(err) });
            defer allocator.free(msg);
            try writeLine(msg);
            continue;
        };

        switch (statement) {
            .element => |parsed| {
                defer parsed.free(allocator);
                const simplified = try simplify.simplify(allocator, parsed);
                defer simplified.free(allocator);
                
                const normalized = try simplify.alphaNormalize(allocator, simplified);
                defer normalized.free(allocator);

                const outstr = try normalized.toString(allocator);
                defer allocator.free(outstr);

                try writeLine(outstr);
            },
            .define => |defn| {
                _ = defn;
                // parsed = defn.element;
            },
            else => continue,
        }
    }
}

fn readLine(allocator: Allocator, reader: std.fs.File.Reader) ![]const u8 {
    var root = try allocator.alloc(u8, 128);
    var len: usize = 0;
    var bufSlice = root;
    
    while (true) {
        const data = reader.readUntilDelimiterOrEof(bufSlice, '\n') catch |err| {
            if (err == error.StreamTooLong) {
                len = root.len;
                root = try allocator.realloc(root, root.len * 2);
                bufSlice = root[len..];
                continue;
            }
            return err;
        };

        if (data) |read| {
            len += read.len;
        }
        return root[0..len];
    }
}

fn writeLine(str: []const u8) !void {
    const stdout = std.io.getStdOut();
    _ = try stdout.write(str);
    _ = try stdout.write("\n");
}

test {
    std.testing.refAllDecls(@This());
}