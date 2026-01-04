//! Zig Test File for UAST-Grep
//! Tests: functions, structs, variables, control flow, error handling

// Single line comment
const std = @import("std");
const mem = std.mem;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

// Constants
const MAX_ITEMS: u32 = 100;
const DEFAULT_NAME = "UAST-Grep";

// Type alias
const ItemList = []const i32;

// Error set
const ProcessError = error{
    OutOfMemory,
    InvalidInput,
    FileNotFound,
    IoError,
};

// Union for result type
const Result = union(enum) {
    ok: []i32,
    err: ProcessError,
};

// Enum
const Status = enum(u16) {
    ok = 200,
    not_found = 404,
    server_error = 500,

    pub fn message(self: Status) []const u8 {
        return switch (self) {
            .ok => "OK",
            .not_found => "Not Found",
            .server_error => "Server Error",
        };
    }
};

// Struct definition
const Person = struct {
    name: []const u8,
    age: u32,
    email: ?[]const u8 = null,

    pub fn isAdult(self: Person) bool {
        return self.age >= 18;
    }
};

// Processor struct
const DataProcessor = struct {
    name: []const u8,
    count: u32 = 0,
    allocator: Allocator,
    cache: std.StringHashMap(i32),

    const Self = @This();

    // Constructor
    pub fn init(allocator: Allocator, name: ?[]const u8) Self {
        return Self{
            .name = name orelse DEFAULT_NAME,
            .allocator = allocator,
            .cache = std.StringHashMap(i32).init(allocator),
        };
    }

    // Destructor
    pub fn deinit(self: *Self) void {
        self.cache.deinit();
    }

    // Process method
    pub fn process(self: *Self, items: ItemList) !ArrayList(i32) {
        var results = ArrayList(i32).init(self.allocator);
        errdefer results.deinit();

        // For loop
        for (items) |item| {
            const transformed = transform(item);
            try results.append(transformed);
        }

        // While loop with index
        var i: usize = 0;
        while (i < items.len) : (i += 1) {
            // Cache the item
            var buf: [32]u8 = undefined;
            const key = std.fmt.bufPrint(&buf, "{d}", .{items[i]}) catch continue;
            self.cache.put(key, items[i]) catch {};
        }

        // For with range
        for (0..10) |_| {
            // Loop body
        }

        self.count = @intCast(results.items.len);
        return results;
    }

    // Log method
    pub fn log(self: Self, message: []const u8) void {
        std.debug.print("[{s}] {s}\n", .{ self.name, message });
    }

    // Risky operation with error handling
    pub fn riskyOperation(self: *Self) !void {
        const file = std.fs.cwd().openFile("test.txt", .{}) catch |err| {
            self.log("Failed to open file");
            return err;
        };
        defer file.close();

        const content = file.readToEndAlloc(self.allocator, 1024 * 1024) catch |err| {
            self.log("Failed to read file");
            return err;
        };
        defer self.allocator.free(content);

        var buf: [64]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "Read {d} bytes", .{content.len}) catch "Read file";
        self.log(msg);
    }
};

// Transform function
fn transform(value: i32) i32 {
    return value * 2;
}

// Generic function
fn mapItems(comptime T: type, items: []const T, func: fn (T) T) ![]T {
    const allocator = std.heap.page_allocator;
    var results = try allocator.alloc(T, items.len);
    for (items, 0..) |item, i| {
        results[i] = func(item);
    }
    return results;
}

// Function with multiple return values (tuple)
fn divmod(numerator: i32, denominator: i32) struct { quotient: i32, remainder: i32 } {
    return .{
        .quotient = @divTrunc(numerator, denominator),
        .remainder = @rem(numerator, denominator),
    };
}

// Inline function
inline fn square(x: i32) i32 {
    return x * x;
}

// Comptime function
fn fibonacci(comptime n: u32) u32 {
    if (n <= 1) return n;
    return fibonacci(n - 1) + fibonacci(n - 2);
}

// Switch expression
fn getStatusMessage(status: Status) []const u8 {
    return switch (status) {
        .ok => "OK",
        .not_found => "Not Found",
        .server_error => "Server Error",
    };
}

// Optional handling
fn processOptional(value: ?i32) i32 {
    // If with capture
    if (value) |v| {
        return v * 2;
    } else {
        return 0;
    }
}

// Orelse pattern
fn getOrDefault(value: ?i32, default: i32) i32 {
    return value orelse default;
}

// Error union handling
fn mayFail(fail: bool) !i32 {
    if (fail) {
        return error.InvalidInput;
    }
    return 42;
}

// Defer and errdefer
fn resourceManagement(allocator: Allocator) !void {
    const buffer = try allocator.alloc(u8, 1024);
    defer allocator.free(buffer);

    // Use buffer...
}

// Test block
test "transform doubles value" {
    try std.testing.expectEqual(@as(i32, 10), transform(5));
}

test "person is adult" {
    const person = Person{ .name = "Alice", .age = 30 };
    try std.testing.expect(person.isAdult());
}

// Main function
pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var processor = DataProcessor.init(allocator, "Main");
    defer processor.deinit();

    const data = [_]i32{ 1, 2, 3, 4, 5 };
    var results = try processor.process(&data);
    defer results.deinit();

    processor.log("Processing complete");

    // Print results
    const stdout = std.io.getStdOut().writer();
    try stdout.print("Results: ", .{});
    for (results.items) |item| {
        try stdout.print("{d} ", .{item});
    }
    try stdout.print("\n", .{});

    // Comptime fibonacci
    const fib10 = comptime fibonacci(10);
    try stdout.print("Fibonacci(10) = {d}\n", .{fib10});

    // Tuple destructuring
    const result = divmod(17, 5);
    try stdout.print("17 / 5 = {d} remainder {d}\n", .{ result.quotient, result.remainder });

    // Person
    const person = Person{ .name = "Alice", .age = 30, .email = "alice@example.com" };
    try stdout.print("{s} is adult: {}\n", .{ person.name, person.isAdult() });

    // Error handling
    const value = mayFail(false) catch |err| {
        std.debug.print("Error: {}\n", .{err});
        return err;
    };
    try stdout.print("Value: {d}\n", .{value});
}
