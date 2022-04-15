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

    var parseState = parser.ParseState.init(allocator);
    defer {
        var keys = parseState.symbolTable.keyIterator();
        while (keys.next()) |key| {
            allocator.free(key.*);
        }
        parseState.deinit();
    }

    var definitions = std.AutoHashMap(elt.Symbol, *const elt.Element(false)).init(allocator);

    while (true) {
        const input = try readLine(allocator, reader);
        defer allocator.free(input);

        // var restorePoint: parser.ParseState = undefined;
        // var freedRestorePoint = false;
        // try parseState.makeSnapshot(&restorePoint);
        // defer if (!freedRestorePoint) {
        //     restorePoint.deinit();
        //     freedRestorePoint = true;
        // };
        // errdefer {
        //     parseState.restoreSnapshot(&restorePoint);
        //     freedRestorePoint = true;
        // }

        const statement = parser.parseStatement(&parseState, input) catch |err| {
            try writeLine(allocator, "Parse error: {s}", .{ @errorName(err) });
            continue;
        };

        switch (statement) {
            .element => |parsed| {
                defer parsed.free(allocator);

                const substituted = simplify.performSubstitutions(allocator, definitions, parsed) catch |err| {
                    try writeLine(allocator, "Substitution error: {s}", .{ @errorName(err) });
                    continue;
                };
                defer substituted.free(allocator);

                const simplified = simplify.simplify(allocator, substituted) catch |err| {
                    try writeLine(allocator, "Simplification error: {s}", .{ @errorName(err) });
                    continue;
                };
                defer simplified.free(allocator);
                
                const normalized = simplify.alphaNormalize(allocator, simplified) catch |err| {
                    try writeLine(allocator, "Normalization error: {s}", .{ @errorName(err) });
                    continue;
                };
                defer normalized.free(allocator);

                const outstr = try normalized.toString(allocator);
                defer allocator.free(outstr);

                try writeLine(allocator, "{s}", .{ outstr });
            },
            .define => |defn| {
                const sym = try parseState.enter(defn.name);
                defer defn.element.free(allocator);

                const simplified = simplify.performSubstitutions(allocator, definitions, defn.element) catch |err| {
                    try writeLine(allocator, "Substitution error: {s}", .{ @errorName(err) });
                    continue;
                };

                try definitions.put(sym, simplified);
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

fn writeLine(allocator: std.mem.Allocator, comptime fmt: []const u8, args: anytype) !void {
    const str = try std.fmt.allocPrint(allocator, fmt ++ "\n", args);
    defer allocator.free(str);
    const stdout = std.io.getStdOut();
    _ = try stdout.write(str);
}

test {
    std.testing.refAllDecls(@This());
}