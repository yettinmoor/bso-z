const std = @import("std");
const print = std.debug.print;

const Cpu = @import("Cpu.zig");

pub fn main() !void {
    print("Under construction!\n", .{});
}

test "modules" {
    _ = @import("Inst.zig");
}
