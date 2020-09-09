const std = @import("std");
const Cpu = @import("src/main.zig").Cpu;

const MemoryTest = struct {
    expected: u8,
    loc: MemoryLoc,

    const MemoryLoc = union(enum) {
        a,
        x,
        y,
        addr: u16,
    };
};

test "basic" {
    const code = &[_]u8{
        0xa0, 0x01, // LDY #$1
        0xa9, 0x05, // LDA #$5
        0x8d, 0x01,
        0x50, // STA
        0x6a, // ROR
        0x79, 0x00,
        0x50, // ADC $5000,Y
        0x60, // RTS
    };

    try runTest(code, &[_]MemoryTest{
        .{ .expected = 0x5, .loc = .{ .addr = 0x5001 } },
        .{ .expected = 0x8, .loc = .a },
    });
}

fn runTest(code: []const u8, tests: []const MemoryTest) !void {
    const entry = 0x8000;
    var memory: [0x10000]u8 = undefined;
    std.mem.copy(u8, memory[entry..], code);

    var cpu = Cpu.init(&memory, entry);
    try cpu.run(.{});

    for (tests) |t| {
        const val = switch (t.loc) {
            .a => cpu.a,
            .x => cpu.x,
            .y => cpu.y,
            .addr => |addr| cpu.memory[addr],
        };
        std.testing.expect(t.expected == val);
    }
}
