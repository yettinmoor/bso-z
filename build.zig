const Builder = @import("std").build.Builder;

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

    const test_files = [_][]const u8{
        "tests/simple",
        "tests/day",
    };

    inline for (test_files) |file| {
        const run_vasm = b.addSystemCommand(&[_][]const u8{
            "vasm6502_oldstyle",
            "-Fbin",
            file ++ ".s",
            "-dotdir",
            "-o",
            file ++ ".bin",
        });
        run_vasm.stdout_action = .ignore;
        test_cmd.dependOn(&run_vasm.step);
    }

    const test_step = b.addTest("test.zig");
    test_cmd.dependOn(&test_step.step);
}
