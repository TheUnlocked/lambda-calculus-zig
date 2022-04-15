const std = @import("std");
const alloc_utils = @import("alloc_utils.zig");
const Symbol = @import("element.zig").Symbol;
const Element = @import("element.zig").Element(false);
const MutableElement = @import("element.zig").Element(true);
const numbers = @import("numbers.zig");

const SimplifyState = struct {
    allocator: std.mem.Allocator,
    changed: bool,
    toFree: std.AutoHashMap(*MutableElement, void),

    fn init(allocator: std.mem.Allocator) SimplifyState {
        return SimplifyState {
            .allocator = allocator,
            .changed = false,
            .toFree = std.AutoHashMap(*MutableElement, void).init(allocator),
        };
    }

    fn deinit(self: *SimplifyState) void {
        self.toFree.deinit();
    }

    fn markMaybeFree(self: *SimplifyState, elt: *MutableElement) std.mem.Allocator.Error!void {
        // Because we do all maybe free marking before we mark anything as not free, 
        // if something is marked maybe free then all its children must be as well,
        // and so we can skip it.
        if (!(try self.toFree.getOrPut(elt)).found_existing) {
            switch (elt.*) {
                .variable => {},
                .lambda => |lam| try self.markMaybeFree(lam.body),
                .apply => |app| {
                    try self.markMaybeFree(app.target);
                    try self.markMaybeFree(app.arg);
                },
            }
        }
    }

    fn markNotFree(self: *SimplifyState, elt: *MutableElement) void {
        _ = self.toFree.remove(elt);
        switch (elt.*) {
            .variable => {},
            .lambda => |lam| self.markNotFree(lam.body),
            .apply => |app| {
                self.markNotFree(app.target);
                self.markNotFree(app.arg);
            },
        }
    }
};

pub fn simplify(allocator: std.mem.Allocator, elt: *const Element) !*const Element {
    var st = SimplifyState.init(allocator);
    defer st.deinit();

    var simplified = try elt.deepClone(allocator, true);
    // try st.markMaybeFree(simplified);

    st.changed = true;
    while (st.changed) {
        st.changed = false;
        try simplifyStep(&st, &simplified);
    }

    st.markNotFree(simplified);
    var toFree = st.toFree.keyIterator();
    while (toFree.next()) |item| {
        st.allocator.destroy(item.*);
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
                    errdefer result.free(st.allocator);

                    if (replace(st, &result, lam.param, app.arg)) {
                        try st.toFree.put(elt.*, {});
                        try st.markMaybeFree(app.target);
                    }
                    else {
                        try st.markMaybeFree(elt.*);
                    }
                    
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

fn replace(st: *SimplifyState, root: **MutableElement, replaceSym: Symbol, with: *MutableElement) bool {
    switch (root.*.*) {
        .variable => |sym| {
            if (sym == replaceSym) {
                st.allocator.destroy(root.*);
                root.* = with;
                return true;
            }
        },
        .lambda => {
            var lam = &root.*.*.lambda;
            if (lam.param != replaceSym) {
                return replace(st, &lam.body, replaceSym, with);
            }
        },
        .apply => {
            var app = &root.*.*.apply;
            const b1 = replace(st, &app.target, replaceSym, with);
            const b2 = replace(st, &app.arg, replaceSym, with);
            return b1 or b2;
        },
    }
    return false;
}

const expect = std.testing.expect;

test "(\\@1 . @1) @0 => @0" {
    const original = try alloc_utils.createWith(std.testing.allocator, Element {
        .apply = .{
            .target = try alloc_utils.createWith(std.testing.allocator, Element {
                .lambda = .{
                    .param = 1,
                    .body = try alloc_utils.createWith(std.testing.allocator, Element { .variable = 1 })
                }
            }),
            .arg = try alloc_utils.createWith(std.testing.allocator, Element { .variable = 0 }),
        }
    });
    defer original.free(std.testing.allocator);

    const result = try simplify(std.testing.allocator, original);
    defer result.free(std.testing.allocator);

    switch (result.*) {
        .variable => |sym| try expect(sym == 0),
        else => return error.Fail,
    }
}

const parser = @import("parser.zig");

test "(snd (pair true false)) => false" {
    const parsed = try parser.parseOneElement(std.testing.allocator, "(\\true.\\false.(\\pair.\\snd. (snd (pair true false))) (\\x.\\y.\\f.f x y) (\\p.p false)) (\\x.\\y.x) (\\x.\\y.y)");
    const simplified = try simplify(std.testing.allocator, parsed);
    parsed.free(std.testing.allocator);
    defer simplified.free(std.testing.allocator);

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

test "Compute factorial 3" {
    const parsed = try parser.parseOneElement(
        std.testing.allocator,
        "(\\fix.\\fact. (fix fact) 3) (\\g.(\\x.g (x x)) (\\x.g (x x))) ((\\iszero.\\mult.\\pred. (\\f.\\n. (iszero n) 1 (mult n (f (pred n))))) (\\n. n (\\x.\\x.\\y.y) (\\x.\\y.x)) (\\m.\\n.\\f.m (n f)) (\\n.\\f.\\x.n (\\g.\\h.h (g f)) (\\u.x) (\\u.u)))"
    );
    const simplified = try simplify(std.testing.allocator, parsed);
    parsed.free(std.testing.allocator);
    defer simplified.free(std.testing.allocator);
    const normalized = try alphaNormalize(std.testing.allocator, simplified);
    defer normalized.free(std.testing.allocator);

    const six = try numbers.makeNumber(std.testing.allocator, 6, .church);
    defer six.free(std.testing.allocator);

    try expect(normalized.equals(six.castImmutable().*));
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

    fn deinit(self: *AlphaNormalizeState) void {
        self.mappings.deinit();
    }
};

pub fn alphaNormalizeMut(allocator: std.mem.Allocator, root: *MutableElement) !void {
    var st = AlphaNormalizeState.init(allocator);
    defer st.deinit();
    try alphaNormalizeMutRec(&st, root);
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
    const parsed = try parser.parseOneElement(std.testing.allocator, "(\\true.\\false.(\\pair.\\snd. (snd (pair true false))) (\\x.\\y.\\f.f x y) (\\p.p false)) (\\x.\\y.x) (\\x.\\y.y)");
    const simplified = try simplify(std.testing.allocator, parsed);
    parsed.free(std.testing.allocator);
    const normalized = try alphaNormalize(std.testing.allocator, simplified);
    simplified.free(std.testing.allocator);
    defer normalized.free(std.testing.allocator);

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