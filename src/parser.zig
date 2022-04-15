const std = @import("std");
const elt = @import("element.zig");
const Element = @import("element.zig").Element(false);
const Statement = @import("statement.zig").Statement;
const alloc_utils = @import("alloc_utils.zig");
const ll_utils = @import("ll_utils.zig");
const numbers = @import("numbers.zig");

const expect = std.testing.expect;

const ParseError = error {
    UnexpectedEndOfInput,
    ExpectedCloseParen,
    UnexpectedOpenParen,
    ExpectedDot,
    IllegalCharacter,
    UnboundIdentifier,
    MissingLeftHandAssignment,
    Internal,
};


pub fn parseStatement(allocator: std.mem.Allocator, str: []const u8) !Statement {
    if (str[0] == '!') {
        // Directives 
    }

    var rest = skipWhitespace(str);

    if (std.mem.indexOfScalar(u8, rest, '=')) |assignmentIndex| {
        // Assignment
        const nameResult = readIdentifier(rest[0..assignmentIndex]);
        
        const name = nameResult.@"0";

        if (name.len == 0) {
            return ParseError.MissingLeftHandAssignment;
        }
        if (skipWhitespace(nameResult.@"1").len != 0) {
            return ParseError.IllegalCharacter;
        }

        rest = str[assignmentIndex + 1..];

        return Statement {
            .define = .{ .name = name, .element = try parseElement(allocator, rest) }
        };
    }

    // Expression
    return Statement {
        .element = try parseElement(allocator, rest),
    };
}

const ParseElementResult = struct {
    elt: *const Element,
    rest: []const u8,
};

const ParseState = struct {
    const LL = std.SinglyLinkedList(struct {
        name: []const u8,
        oldSym: ?elt.Symbol = null,
    });
    const HT = std.StringHashMap(elt.Symbol);

    allocator: std.mem.Allocator,
    symbolIterator: elt.Symbol = 0,
    symbolTable: HT,
    restoreStack: LL = .{},

    fn init(allocator: std.mem.Allocator) ParseState {
        return ParseState {
            .allocator = allocator,
            .symbolTable = HT.init(allocator),
        };
    }

    fn deinit(self: *ParseState) void {
        ll_utils.free(self.allocator, &self.restoreStack);
        self.symbolTable.deinit();
    }

    /// Snapshots can only be used once. If a snapshot is unused, it should be deinitialized explicitly.
    /// Snapshots should not be deinitialized if they are used.
    pub fn makeSnapshot(self: ParseState, snapshot: *ParseState) !*ParseState {
        snapshot.* = self;
        snapshot.symbolTable = try snapshot.symbolTable.clone();
        snapshot.restoreStack = try ll_utils.clone(self.allocator, self.restoreStack);
    }

    pub fn restoreSnapshot(self: *ParseState, snapshot: *ParseState) void {
        self.deinit();
        self.* = snapshot.*;
    }

    fn enter(self: *ParseState, name: []const u8) !elt.Symbol {
        const sym = self.symbolIterator;

        var node: *LL.Node = undefined;

        if (self.symbolTable.get(name)) |oldSym| {
            node = try alloc_utils.createWith(self.allocator, LL.Node { .data = .{
                .name = name,
                .oldSym = oldSym,
            } });

            self.restoreStack.prepend(node);
        }
        else {
            node = try alloc_utils.createWith(self.allocator, LL.Node { .data = .{
                .name = name,
            } });

            self.restoreStack.prepend(node);
        }
        errdefer self.allocator.destroy(node);

        try self.symbolTable.put(name, sym);
        
        self.symbolIterator += 1;
        return sym;
    }

    fn exit(self: *ParseState) !void {
        if (self.restoreStack.popFirst()) |symInfoPtr| {
            defer self.allocator.destroy(symInfoPtr);
            
            const symInfo = symInfoPtr.*.data;
            const name = symInfo.name;
            if (symInfo.oldSym) |oldSym| {
                try self.symbolTable.put(name, oldSym);
            }
            else {
                _ = self.symbolTable.remove(name);
            }
        }
        else {
            return ParseError.Internal;
        }
    }

    fn getSymbolFromName(self: ParseState, name: []const u8) ?elt.Symbol {
        return self.symbolTable.get(name);
    }

    fn getNewSymbol(self: ParseState) elt.Symbol {
        const sym = self.symbolIterator;
        self.symbolIterator += 1;
        return sym;
    }
};

pub fn parseElement(allocator: std.mem.Allocator, str: []const u8) !*const Element {
    var st = ParseState.init(allocator);
    defer st.deinit();
    return (try parseRoot(&st, str)).elt;
}

test "parse '\\x. (\\x. x) x'" {
    const result = try parseElement(std.testing.allocator, "\\x. (\\x. x) x");
    defer result.free(std.testing.allocator);

    switch (result.*) {
        .lambda => |lam1| {
            try expect(lam1.param == 0);
            switch (lam1.body.*) {
                .apply => |app| {
                    switch (app.target.*) {
                        .lambda => |lam2| {
                            try expect(lam2.param == 1);
                            switch (lam2.body.*) {
                                .variable => |sym| try expect(sym == 1),
                                else => return error.Fail,
                            }
                        },
                        else => return error.AppFunction,
                    }
                    switch (app.arg.*) {
                        .variable => |sym| try expect(sym == 0),
                        else => return error.Fail,
                    }
                },
                else => return error.Fail,
            }
        },
        else => return error.Fail,
    }
}

test "parse '\\x. \\a. a x x'" {
    const result = try parseElement(std.testing.allocator, "\\x. \\a. a x x");
    defer result.free(std.testing.allocator);

    switch (result.*) {
        .lambda => |lam1| {
            try expect(lam1.param == 0);
            switch (lam1.body.*) {
                .lambda => |lam2| {
                    try expect(lam2.param == 1);
                    switch (lam2.body.*) {
                        .apply => |app1| {
                            switch (app1.target.*) {
                                .apply => |app2| {
                                    switch (app2.target.*) {
                                        .variable => |sym| try expect(sym == 1),
                                        else => return error.Fail,
                                    }
                                    switch (app2.arg.*) {
                                        .variable => |sym| try expect(sym == 0),
                                        else => return error.Fail,
                                    }
                                },
                                else => return error.Fail,
                            }
                            switch (app1.arg.*) {
                                .variable => |sym| try expect(sym == 0),
                                else => return error.Fail,
                            }
                        },
                        else => return error.Fail,
                    }
                },
                else => return error.Fail,
            }
        },
        else => return error.Fail,
    }
}

test "parse '\\x. \\y. \\a. a x y'" {
    const result = try parseElement(std.testing.allocator, "\\x. \\y. \\a. a x y");
    defer result.free(std.testing.allocator);

    switch (result.*) {
        .lambda => |lam1| {
            try expect(lam1.param == 0);
            switch (lam1.body.*) {
                .lambda => |lam2| {
                    try expect(lam2.param == 1);
                    switch (lam2.body.*) {
                        .lambda => |lam3| {
                            try expect(lam3.param == 2);
                            switch (lam3.body.*) {
                                .apply => |app1| {
                                    switch (app1.target.*) {
                                        .apply => |app2| {
                                            switch (app2.target.*) {
                                                .variable => |sym| try expect(sym == 2),
                                                else => return error.Fail,
                                            }
                                            switch (app2.arg.*) {
                                                .variable => |sym| try expect(sym == 0),
                                                else => return error.Fail,
                                            }
                                        },
                                        else => return error.Fail,
                                    }
                                    switch (app1.arg.*) {
                                        .variable => |sym| try expect(sym == 1),
                                        else => return error.Fail,
                                    }
                                },
                                else => return error.Fail,
                            }
                        },
                        else => return error.Fail,
                    }
                },
                else => return error.Fail,
            }
        },
        else => return error.Fail,
    }
}

fn skipWhitespace(str: []const u8) []const u8 {
    var offset: usize = 0;
        
    while (offset < str.len and std.ascii.isSpace(str[offset])) {
        offset += 1;
    }

    return str[offset..];
}

fn parseRoot(st: *ParseState, _str: []const u8) (std.mem.Allocator.Error || ParseError)!ParseElementResult {
    var str = skipWhitespace(_str);

    if (str.len == 0) {
        return ParseError.UnexpectedEndOfInput;
    }

    var left = try parseNonApplication(st, str);
    str = skipWhitespace(left.rest);

    var result = left.elt;

    
    const EltStack = std.TailQueue(*const Element);
    var ll = EltStack {};
    errdefer {
        // Just in case there's an error, clean everything up
        while (ll.popFirst()) |node| {
            st.allocator.destroy(node);
        }
    }

    while (str.len != 0 and str[0] != ')') {
        const next = try parseNonApplication(st, str);

        ll.prepend(try alloc_utils.createWith(st.allocator, EltStack.Node { .data = next.elt }));

        str = skipWhitespace(next.rest);
    }

    while (ll.pop()) |node| {
        defer st.allocator.destroy(node);
        result = try alloc_utils.createWith(st.allocator, Element {
            .apply = .{
                .target = result,
                .arg = node.data,
            }
        });
    }
    else {
        return ParseElementResult {
            .elt = result,
            .rest = str
        };
    }
}

fn parseNonApplication(st: *ParseState, str: []const u8) !ParseElementResult {
    return switch (str[0]) {
        '(' => paren: {
            var rest = str[1..];
            var result = try parseRoot(st, rest);
            rest = skipWhitespace(result.rest);
            
            if (rest[0] != ')') {
                break :paren ParseError.ExpectedCloseParen;
            }

            break :paren ParseElementResult {
                .elt = result.elt,
                .rest = rest[1..],
            };
        },
        ')' => ParseError.UnexpectedOpenParen,
        '\\' => parseLambda(st, str),
        'a'...'z', '_' => parseIdentifier(st, str),
        '0'...'9' => parseNum(st, str),
        else => ParseError.IllegalCharacter
    };
}

fn isLegalChar(char: u8) bool {
    return std.ascii.isAlNum(char) or char == '_';
}

fn readIdentifier(str: []const u8) std.meta.Tuple(&[_]type { []const u8, []const u8 }) {
    var offset: usize = 1;

    while (offset < str.len and isLegalChar(str[offset])) {
        offset += 1;
    }

    return .{ str[0..offset], str[offset..] };
}

fn parseLambda(st: *ParseState, str: []const u8) !ParseElementResult {
    const identInfo = readIdentifier(skipWhitespace(str[1..]));

    var rest = skipWhitespace(identInfo.@"1");

    if (rest.len == 0) {
        return ParseError.UnexpectedEndOfInput;
    }

    if (rest[0] != '.') {
        return ParseError.ExpectedDot;
    }

    rest = rest[1..];

    const sym = try st.enter(identInfo.@"0");
    defer st.exit() catch unreachable;

    const body = try parseRoot(st, rest);

    return ParseElementResult {
        .elt = try alloc_utils.createWith(st.allocator, Element {
            .lambda = .{ .param = sym, .body = body.elt }
        }),
        .rest = body.rest,
    };
}

test "parseLambda \\x. x" {
    var st = ParseState.init(std.testing.allocator);
    defer st.deinit();

    const result = try parseLambda(&st, "\\x. x");
    defer result.elt.free(std.testing.allocator);

    switch (result.elt.*) {
        .lambda => |lam| {
            try expect(lam.param == 0);
            switch (lam.body.*) {
                .variable => |value| {
                    try expect(value == 0);
                },
                else => return error.Fail,
            }
        },
        else => return error.Fail,
    }
}

test "parseLambda \\x. a when a already exists" {
    var st = ParseState.init(std.testing.allocator);
    defer st.deinit();

    _ = try st.enter("a");
    const result = try parseLambda(&st, "\\x. a");
    defer result.elt.free(std.testing.allocator);

    switch (result.elt.*) {
        .lambda => |lam| {
            try expect(lam.param == 1);
            switch (lam.body.*) {
                .variable => |value| try expect(value == 0),
                else => return error.Fail,
            }
        },
        else => return error.Fail,
    }
}

test "parseLambda \\x. x when x already exists" {
    var st = ParseState.init(std.testing.allocator);
    defer st.deinit();

    _ = try st.enter("x");
    const result = try parseLambda(&st, "\\x. x");
    defer result.elt.free(std.testing.allocator);

    switch (result.elt.*) {
        .lambda => |lam| {
            try expect(lam.param == 1);
            switch (lam.body.*) {
                .variable => |value| try expect(value == 1),
                else => return error.Fail,
            }
        },
        else => return error.Fail,
    }
}

fn parseIdentifier(st: *const ParseState, str: []const u8) !ParseElementResult {
    const identResult = readIdentifier(str);

    const sym = st.getSymbolFromName(identResult.@"0") orelse return ParseError.UnboundIdentifier;

    return ParseElementResult {
        .elt = try alloc_utils.createWith(st.allocator, Element { .variable = sym }),
        .rest = identResult.@"1"
    };
}

test "parseName with eof" {
    var st = ParseState.init(std.testing.allocator);
    defer st.deinit();
    
    _ = try st.enter("a");
    const result = try parseIdentifier(&st, "a");
    defer result.elt.free(std.testing.allocator);
    
    try std.testing.expectEqualStrings(result.rest, "");

    switch (result.elt.*) {
        .variable => |value| try expect(value == 0),
        else => return error.Fail,
    }
}

test "parseName without eof" {
    var st = ParseState.init(std.testing.allocator);
    defer st.deinit();

    _ = try st.enter("a");
    const result = try parseIdentifier(&st, "a (\\x. a x)");
    defer result.elt.free(std.testing.allocator);

    try std.testing.expectEqualStrings(result.rest, " (\\x. a x)");
    
    switch (result.elt.*) {
        .variable => {},
        else => return error.Fail,
    }
}

test "parseName with longer names" {
    var st = ParseState.init(std.testing.allocator);
    defer st.deinit();
    
    _ = try st.enter("longer_1_name_23");
    const result = try parseIdentifier(&st, "longer_1_name_23");
    defer result.elt.free(std.testing.allocator);
    
    switch (result.elt.*) {
        .variable => {},
        else => return error.Fail,
    }
}

test "parseName when name isn't present" {
    var st = ParseState.init(std.testing.allocator);
    defer st.deinit();
    try std.testing.expectError(ParseError.UnboundIdentifier, parseIdentifier(&st, "a"));
}

fn readNum(str: []const u8) std.meta.Tuple(&[_]type { []const u8, []const u8 }) {
    var offset: usize = 1;

    while (offset < str.len and std.ascii.isDigit(str[offset])) {
        offset += 1;
    }

    return .{ str[0..offset], str[offset..] };
}

fn parseNum(st: *const ParseState, str: []const u8) !ParseElementResult {
    const numResult = readNum(str);

    const n = std.fmt.parseInt(u32, numResult.@"0", 10) catch return ParseError.Internal;
    const num = try numbers.makeNumber(st.allocator, n, .church);

    return ParseElementResult {
        .elt = num.castImmutable(),
        .rest = numResult.@"1"
    };
}
