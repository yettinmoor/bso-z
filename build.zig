const std = @import("std");
const test_runner = @import("test_runner.zig");
const Builder = std.build.Builder;

pub fn build(b: *Builder) void {
    const target = b.standardTargetOptions(.{});
    const mode = b.standardReleaseOptions();

    const exe = b.addExecutable("bsoz", "src/main.zig");
    exe.setTarget(target);
    exe.setBuildMode(mode);
    exe.install();

    const run_cmd = exe.run();
    run_cmd.step.dependOn(b.getInstallStep());

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    const test_cmd = b.step("test", "Run tests on files in tests/");

    inline for (test_runner.tests) |t| {
        const filename = t.filename;
        const run_vasm = b.addSystemCommand(&[_][]const u8{
            "vasm6502_oldstyle",
            "-Fbin",
            "tests/" ++ filename ++ ".s",
            "-dotdir",
            "-o",
            "tests/" ++ filename ++ ".bin",
        });
        run_vasm.stdout_action = .ignore;
        test_cmd.dependOn(&run_vasm.step);
    }

    const test_step = b.addTest("test_runner.zig");
    test_cmd.dependOn(&test_step.step);
}
