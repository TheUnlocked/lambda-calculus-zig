const std = @import("std");
const alloc_utils = @import("alloc_utils.zig");
const Symbol = @import("element.zig").Symbol;
const Element = @import("element.zig").Element(false);
const MutableElement = @import("element.zig").Element(true);

const SimplifyState = struct {
    allocator: std.mem.Allocator,
    changed: bool,

    fn init(allocator: std.mem.Allocator) SimplifyState {
        return SimplifyState {
            .allocator = allocator,
            .changed = false,
        };
    }
};

pub fn simplify(allocator: std.mem.Allocator, elt: *const Element) !*const Element {
    var st = SimplifyState.init(allocator);

    var simplified = try elt.deepClone(allocator, true);

    st.changed = true;
    while (st.changed) {
        st.changed = false;
        try simplifyStep(&st, &simplified);
    }

    return simplified.castImmutable();
}

fn simplifyStep(st: *SimplifyState, elt: **MutableElement) std.mem.Allocator.Error!void {
    switch (elt.*.*) {
        .variable => {},
        .lambda => {
            try simplifyStep(st, &elt.*.*.lambda.body);
        },
        .apply => |app| {
            var appPtr = &elt.*.*.apply;
            switch (app.target.*) {
                .variable => try simplifyStep(st, &appPtr.arg),
                .lambda => |lam| {
                    var result = try lam.body.deepClone(st.allocator, true);
                    replace(st, &result, lam.param, app.arg);
                    elt.* = result;
                    st.changed = true;
                },
                .apply => {
                    try simplifyStep(st, &appPtr.target);
                    if (!st.changed) {
                        try simplifyStep(st, &appPtr.arg);
                    }
                },
            }
        },
    }
}

fn replace(st: *SimplifyState, root: **MutableElement, replaceSym: Symbol, with: *MutableElement) void {
    switch (root.*.*) {
        .variable => |sym| {
            if (sym == replaceSym) {
                root.* = with;
            }
        },
        .lambda => {
            var lam = &root.*.*.lambda;
            if (lam.param != replaceSym) {
                replace(st, &lam.body, replaceSym, with);
            }
        },
        .apply => {
            var app = &root.*.*.apply;
            replace(st, &app.target, replaceSym, with);
            replace(st, &app.arg, replaceSym, with);
        },
    }
}

const expect = std.testing.expect;

test "(\\@1 . @1) @0 => @0" {
    const result = try simplify(std.testing.allocator, try alloc_utils.createWith(std.testing.allocator, Element {
        .apply = .{
            .target = try alloc_utils.createWith(std.testing.allocator, Element {
                .lambda = .{
                    .param = 1,
                    .body = try alloc_utils.createWith(std.testing.allocator, Element { .variable = 1 })
                }
            }),
            .arg = try alloc_utils.createWith(std.testing.allocator, Element { .variable = 0 }),
        }
    }));

    switch (result.*) {
        .variable => |sym| try expect(sym == 0),
        else => return error.Fail,
    }
}

const parser = @import("parser.zig");

test "(snd (pair true false)) => false" {
    const parsed = try parser.parse(std.testing.allocator, "(\\true.\\false.(\\pair.\\snd. (snd (pair true false))) (\\x.\\y.\\f.f x y) (\\p.p false)) (\\x.\\y.x) (\\x.\\y.y)");
    const simplified = try simplify(std.testing.allocator, parsed);
    parsed.free(std.testing.allocator);

    switch (simplified.*) {
        .lambda => |lam1| {
            switch (lam1.body.*) {
                .lambda => |lam2| {
                    try expect(lam1.param != lam2.param);
                    switch (lam2.body.*) {
                        .variable => |sym| try expect(sym == lam2.param),
                        else => return error.Fail,
                    }
                },
                else => return error.Fail,
            }
        },
        else => return error.Fail,
    }
}

pub fn alphaNormalize(allocator: std.mem.Allocator, elt: *const Element) !*const Element {
    const copy = try elt.deepClone(allocator, true);
    errdefer copy.free(allocator);
    try alphaNormalizeMut(allocator, copy);
    return copy.castImmutable();
}

const AlphaNormalizeState = struct {
    mappings: std.AutoHashMap(Symbol, Symbol),
    counter: Symbol = 0,

    fn init(allocator: std.mem.Allocator) AlphaNormalizeState {
        return AlphaNormalizeState {
            .mappings = std.AutoHashMap(Symbol, Symbol).init(allocator),
        };
    }
};

pub fn alphaNormalizeMut(allocator: std.mem.Allocator, root: *MutableElement) !void {
    try alphaNormalizeMutRec(&AlphaNormalizeState.init(allocator), root);
}

fn alphaNormalizeMutRec(st: *AlphaNormalizeState, elt: *MutableElement) std.mem.Allocator.Error!void {
    switch (elt.*) {
        .variable => |sym| {
            if (st.mappings.get(sym)) |newSym| {
                elt.*.variable = newSym;
            }
            else {
                const newSym = st.counter;
                try st.mappings.put(sym, newSym);
                elt.*.variable = newSym;
                st.counter += 1;
            }
        },
        .lambda => |lam| {
            if (st.mappings.get(lam.param)) |newParam| {
                elt.*.lambda.param = newParam;
            }
            else {
                const newParam = st.counter;
                try st.mappings.put(lam.param, newParam);
                elt.*.lambda.param = newParam;
                st.counter += 1;
            }
            try alphaNormalizeMutRec(st, lam.body);
        },
        .apply => |app| {
            try alphaNormalizeMutRec(st, app.target);
            try alphaNormalizeMutRec(st, app.arg);
        },
    }
}

test "(snd (pair true false)) normalizes to \\@0. \\@1. @1" {
    const parsed = try parser.parse(std.testing.allocator, "(\\true.\\false.(\\pair.\\snd. (snd (pair true false))) (\\x.\\y.\\f.f x y) (\\p.p false)) (\\x.\\y.x) (\\x.\\y.y)");
    const simplified = try simplify(std.testing.allocator, parsed);
    parsed.free(std.testing.allocator);
    const normalized = try alphaNormalize(std.testing.allocator, simplified);
    simplified.free(std.testing.allocator);

    switch (normalized.*) {
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