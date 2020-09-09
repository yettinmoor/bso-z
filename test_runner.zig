const std = @import("std");
const Cpu = @import("src/Cpu.zig");

const Test = struct {
    /// Without .bin extension
    filename: []const u8,
    file_entry: usize = 0x8000,
    mem_tests: []const MemTest,
};

const MemTest = struct {
    input: TestInput,
    expected_output: []const TestOutput,

    const TestInput = struct {
        a: u8 = 0,
        x: u8 = 0,
        y: u8 = 0,
    };

    const TestOutput = struct {
        val: u8,
        loc: MemLoc,
    };

    const MemLoc = union(enum) {
        a,
        x,
        y,
        addr: u16,
    };
};

// Imported by build.zig
pub const tests = [_]Test{
    .{
        .filename = "simple",
        .mem_tests = &[_]MemTest{
            .{
                .input = .{},
                .expected_output = &[_]MemTest.TestOutput{
                    .{ .val = 0x5, .loc = .{ .addr = 0x5001 } },
                    .{ .val = 0x8, .loc = .a },
                },
            },
        },
    },
    .{
        .filename = "day",
        .mem_tests = &[_]MemTest{
            .{
                .input = .{ .a = 7, .x = 9, .y = 120 },
                .expected_output = &[_]MemTest.TestOutput{
                    .{ .val = 0x1, .loc = .a },
                },
            },
            .{
                .input = .{ .a = 24, .x = 12, .y = 121 },
                .expected_output = &[_]MemTest.TestOutput{
                    .{ .val = 0x5, .loc = .a },
                },
            },
        },
    },
    .{
        .filename = "clean",
        .file_entry = 0x0,
        .mem_tests = &[_]MemTest{
            .{
                .input = .{ .x = 0xff },
                .expected_output = &[_]MemTest.TestOutput{
                    .{ .val = 0, .loc = .x },
                    .{ .val = 0, .loc = .{ .addr = 0x5000 } },
                    .{ .val = 0, .loc = .{ .addr = 0x50ff } },
                    .{ .val = 0xea, .loc = .{ .addr = 0x4fff } },
                    .{ .val = 0xea, .loc = .{ .addr = 0x5100 } },
                },
            },
        },
    },
};

test "sample files" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = &gpa.allocator;

    const test_dir = try std.fs.cwd().openDir("tests", .{ .iterate = true });

    std.debug.print("\n", .{});
    inline for (tests) |t| {
        std.debug.print("Testing {}...", .{t.filename ++ ".bin"});

        const code = try test_dir.readFileAlloc(
            allocator,
            t.filename ++ ".bin",
            std.math.maxInt(usize),
        );

        const entry = 0x8000;
        var memory: [0x10000]u8 = undefined;
        std.mem.copy(u8, memory[t.file_entry..], code);

        var cpu = Cpu.init(&memory, entry);

        inline for (t.mem_tests) |mem_test| {
            cpu.reset();
            cpu.a = mem_test.input.a;
            cpu.x = mem_test.input.x;
            cpu.y = mem_test.input.y;
            try cpu.run(.{});

            inline for (mem_test.expected_output) |ex| {
                const val = switch (ex.loc) {
                    .a => cpu.a,
                    .x => cpu.x,
                    .y => cpu.y,
                    .addr => |addr| cpu.memory[addr],
                };
                if (ex.val != val) {
                    switch (ex.loc) {
                        .a, .x, .y => std.debug.print(
                            "Failure: expected 0x{x} at register {}, got 0x{x}\n",
                            .{ ex.val, @tagName(ex.loc), val },
                        ),
                        .addr => |addr| std.debug.print(
                            "Failure: expected 0x{x} at memory address {x}, got 0x{x}\n",
                            .{ ex.val, addr, val },
                        ),
                    }
                    std.testing.expect(false);
                }
            }
        }
        std.debug.print("OK\n", .{});
    }
}
