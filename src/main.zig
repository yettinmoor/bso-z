const std = @import("std");
const mem = std.mem;
const assert = std.debug.assert;
const print = std.debug.print;

const Inst = @import("Inst.zig");

pub const Cpu = struct {
    pc: u16,
    a: u8,
    x: u8,
    y: u8,
    sp: u8,
    sr: StatusRegister,

    memory: []u8,
    entry_point: u16,

    const stack_top = 0x0200;
    const stack_bottom = 0x0100;

    const StatusRegister = packed struct { // NVsBDIZC
        carry: u1,
        zero: u1,
        interrupt_disable: u1,
        decimal: u1,
        @"break": u1,
        _: u1 = 0,
        overflow: u1,
        negative: u1,

        fn asByte(self: StatusRegister) u8 {
            return @bitCast(u8, self);
        }

        fn set(self: *StatusRegister, val: u8) void {
            self.* = @bitCast(StatusRegister, val);
        }
    };

    pub fn init(memory: []u8, entry_point: u16) Cpu {
        var cpu: Cpu = undefined;
        cpu.memory = memory;
        cpu.entry_point = entry_point;
        cpu.reset();
        return cpu;
    }

    pub fn reset(self: *Cpu) void {
        self.pc = self.entry_point;
        self.a = 0;
        self.x = 0;
        self.y = 0;
        self.sp = 0xff;
        self.sr.set(0);
    }

    pub fn run(self: *Cpu, opts: RunOptions) Error!void {
        while (true) {
            const inst = try Inst.fetch(self.memory[self.pc..]);
            defer if (opts.print != .no) {
                self.printState(inst, opts);
            };
            self.step(inst) catch |err| switch (err) {
                // TODO: for now, we exit when returning from a subroutine
                // when no return address is on the stack
                Error.StackUnderflowReturnFromSubroutine => return,
                Error.InvalidOpcode => return err,
            };
        }
    }

    /// Run ONE instruction
    pub fn step(self: *Cpu, inst: Inst) Error!void {
        defer self.pc += inst.size();
        const op = inst.op;

        // Addresses must be recalculated in the case of certain addressing modes.
        // We don't do this when fetching in order to preserve the original disassembly
        // for other use cases, e.g. printing.
        const operand: Inst.Operand = switch (inst.operand) {
            .addr => .{ .addr = self.resolveAddress(inst) },
            else => inst.operand,
        };

        switch (op) {
            .adc => {
                const val = switch (operand) {
                    .imm => |imm| imm,
                    .addr => |addr| self.memory[addr],
                    else => unreachable,
                };
                const sign = self.a & 0x80;
                const is_carry = @addWithOverflow(u8, self.a, val, &self.a);
                // Maximum addition is 0xff + 0xff = 0x1fe, so adding carry cannot overflow
                self.a += self.carry();
                if (is_carry) self.sr.carry = 1;
                self.sr.negative = @boolToInt(self.a & 0x80 != 0);
                self.sr.zero = @boolToInt(self.a == 0);
                self.sr.overflow = @boolToInt(sign != (self.a & 0x80));
            },
            .@"and" => {
                const val = switch (operand) {
                    .imm => |imm| imm,
                    .addr => |addr| self.memory[addr],
                    else => unreachable,
                };
                self.a &= val;
                self.sr.negative = @boolToInt(self.a & 0x80 != 0);
                self.sr.zero = @boolToInt(self.a == 0);
            },
            .asl => {
                const target = switch (operand) {
                    .addr => |addr| &self.memory[addr],
                    .impl => &self.a,
                    else => unreachable,
                };
                self.sr.carry = @boolToInt(@shlWithOverflow(u8, target.*, 1, target));
                self.sr.negative = @boolToInt(target.* & 0x80 != 0);
                self.sr.zero = @boolToInt(target.* == 0);
            },
            .bcc => if (self.sr.carry == 0) self.jumpBy(operand.rel),
            .bcs => if (self.sr.carry == 1) self.jumpBy(operand.rel),
            .beq => if (self.sr.zero == 1) self.jumpBy(operand.rel),
            .bit => {
                // N receives the initial, un-ANDed value of memory bit 7.
                // V receives the initial, un-ANDed value of memory bit 6.
                // Z is set if the result of the AND is zero, otherwise reset.
                const val = self.memory[operand.addr];
                self.sr.negative = @boolToInt(val & 0x80 != 0);
                self.sr.overflow = @boolToInt(val & 0x40 != 0);
                self.sr.zero = @boolToInt(self.a & val == 0);
            },
            .bmi => if (self.sr.negative == 1) self.jumpBy(operand.rel),
            .bne => if (self.sr.zero == 0) self.jumpBy(operand.rel),
            .bpl => if (self.sr.negative == 0) self.jumpBy(operand.rel),
            // .brk,
            .bvc => if (self.sr.overflow == 0) self.jumpBy(operand.rel),
            .bvs => if (self.sr.overflow == 1) self.jumpBy(operand.rel),
            .clc => self.sr.carry = 0,
            .cld => self.sr.decimal = 0,
            .cli => self.sr.interrupt_disable = 0,
            .clv => self.sr.overflow = 0,
            .cmp, .cpx, .cpy => {
                // reg < memory => N
                // reg = memory => ZC
                // reg > memory => C
                const val = switch (operand) {
                    .imm => |imm| imm,
                    .addr => |addr| self.memory[addr],
                    else => unreachable,
                };
                const compare_to = switch (op) {
                    .cmp => self.a,
                    .cpx => self.x,
                    .cpy => self.y,
                    else => unreachable,
                };
                switch (std.math.order(compare_to, val)) {
                    .lt => {
                        self.sr.negative = 1;
                        self.sr.carry = 0;
                    },
                    .eq => {
                        self.sr.negative = 1;
                        self.sr.carry = 1;
                    },
                    .gt => {
                        self.sr.negative = 0;
                        self.sr.carry = 1;
                    },
                }
            },
            .dec => {
                self.memory[operand.addr] -%= 1;
                self.sr.negative = @boolToInt(self.memory[operand.addr] & 0x80 != 0);
                self.sr.zero = @boolToInt(self.memory[operand.addr] == 0);
            },
            .dex => {
                self.x -%= 1;
                self.sr.negative = @boolToInt(self.x & 0x80 != 0);
                self.sr.zero = @boolToInt(self.x == 0);
            },
            .dey => {
                self.y -%= 1;
                self.sr.negative = @boolToInt(self.y & 0x80 != 0);
                self.sr.zero = @boolToInt(self.y == 0);
            },
            .eor => {
                const val = switch (operand) {
                    .imm => |imm| imm,
                    .addr => |addr| self.memory[addr],
                    else => unreachable,
                };
                self.a ^= val;
                self.sr.negative = @boolToInt(self.a & 0x80 != 0);
                self.sr.zero = @boolToInt(self.a == 0);
            },
            .inc => {
                self.memory[operand.addr] += 1;
                const val = self.memory[operand.addr];
                self.sr.negative = @boolToInt(val & 0x80 != 0);
                self.sr.zero = @boolToInt(val == 0);
            },
            .inx => {
                self.x += 1;
                self.sr.negative = @boolToInt(self.x & 0x80 != 0);
                self.sr.zero = @boolToInt(self.x == 0);
            },
            .iny => {
                self.y += 1;
                self.sr.negative = @boolToInt(self.y & 0x80 != 0);
                self.sr.zero = @boolToInt(self.y == 0);
            },
            .jmp => self.pc = operand.addr - inst.size(),
            .jsr => {
                self.push(u16, self.pc + inst.size() - 1); // rts pulls this addr and adds 1
                self.pc = operand.addr - inst.size();
            },
            .lda => {
                self.a = switch (operand) {
                    .imm => |imm| imm,
                    .addr => |addr| self.memory[addr],
                    .impl => 1,
                    else => unreachable,
                };
                self.sr.negative = @boolToInt(self.a & 0x80 != 0);
                self.sr.zero = @boolToInt(self.a == 0);
            },
            .ldx => {
                self.x = switch (operand) {
                    .imm => |imm| imm,
                    .addr => |addr| self.memory[addr],
                    .impl => 1,
                    else => unreachable,
                };
                self.sr.negative = @boolToInt(self.x & 0x80 != 0);
                self.sr.zero = @boolToInt(self.x == 0);
            },
            .ldy => {
                self.y = switch (operand) {
                    .imm => |imm| imm,
                    .addr => |addr| self.memory[addr],
                    .impl => 1,
                    else => unreachable,
                };
                self.sr.negative = @boolToInt(self.y & 0x80 != 0);
                self.sr.zero = @boolToInt(self.y == 0);
            },
            .lsr => {
                const target = switch (operand) {
                    .addr => |addr| &self.memory[addr],
                    .impl => &self.a,
                    else => unreachable,
                };
                self.sr.carry = @truncate(u1, target.*);
                target.* = (target.* >> 1);
                self.sr.negative = @boolToInt(target.* & 0x80 != 0);
                self.sr.zero = @boolToInt(target.* == 0);
            },
            .nop => {},
            .ora => {
                const val = switch (operand) {
                    .imm => |imm| imm,
                    .addr => |addr| self.memory[addr],
                    else => unreachable,
                };
                self.a |= val;
                self.sr.negative = @boolToInt(self.a & 0x80 != 0);
                self.sr.zero = @boolToInt(self.a == 0);
            },
            .pha => self.push(u8, self.a),
            .php => self.push(u8, self.sr.asByte()),
            .pla => {
                self.a = self.pull(u8);
                self.sr.negative = @boolToInt(self.a & 0x80 != 0);
                self.sr.zero = @boolToInt(self.a == 0);
            },
            .plp => self.sr.set(self.pull(u8)),
            .rol => {
                const target = switch (operand) {
                    .addr => |addr| &self.memory[addr],
                    .impl => &self.a,
                    else => unreachable,
                };
                const is_carry = @boolToInt(@shlWithOverflow(u8, target.*, 1, target));
                target.* |= self.carry();
                self.sr.carry = is_carry;
                self.sr.negative = @boolToInt(target.* & 0x80 != 0);
                self.sr.zero = @boolToInt(target.* == 0);
            },
            .ror => {
                const target = switch (operand) {
                    .addr => |addr| &self.memory[addr],
                    .impl => &self.a,
                    else => unreachable,
                };
                const is_carry = @truncate(u1, target.*);
                target.* = (target.* >> 1);
                self.sr.carry = is_carry;
                self.sr.negative = @boolToInt(target.* & 0x80 != 0);
                self.sr.zero = @boolToInt(target.* == 0);
            },
            // .rti,
            .rts => {
                if (self.sp == 0xff)
                    return error.StackUnderflowReturnFromSubroutine;
                self.pc = self.pull(u16);
            },
            .sbc => {
                const val = switch (operand) {
                    .imm => |imm| imm,
                    .addr => |addr| self.memory[addr],
                    else => unreachable,
                };
                const sign = self.a & 0x80;
                const is_carry = @subWithOverflow(u8, self.a, val, &self.a);
                // Subtraction sets carry and clears on overflow
                self.a -= ~self.carry();
                self.sr.negative = @boolToInt(self.a & 0x80 != 0);
                self.sr.zero = @boolToInt(self.a == 0);
                self.sr.overflow = @boolToInt(sign != (self.a & 0x80));
            },
            .sec => self.sr.carry = 1,
            .sed => self.sr.decimal = 1,
            .sei => self.sr.interrupt_disable = 1,
            .sta => self.memory[operand.addr] = self.a,
            .stx => self.memory[operand.addr] = self.x,
            .sty => self.memory[operand.addr] = self.y,
            .tax => {
                self.x = self.a;
                self.sr.negative = @boolToInt(self.x & 0x80 != 0);
                self.sr.zero = @boolToInt(self.x == 0);
            },
            .tsx => {
                self.x = self.sp;
                self.sr.negative = @boolToInt(self.x & 0x80 != 0);
                self.sr.zero = @boolToInt(self.x == 0);
            },
            .txa => {
                self.a = self.x;
                self.sr.negative = @boolToInt(self.a & 0x80 != 0);
                self.sr.zero = @boolToInt(self.a == 0);
            },
            .txs => {
                self.sp = self.x;
                self.sr.negative = @boolToInt(self.sp & 0x80 != 0);
                self.sr.zero = @boolToInt(self.sp == 0);
            },
            .tya => {
                self.a = self.y;
                self.sr.negative = @boolToInt(self.a & 0x80 != 0);
                self.sr.zero = @boolToInt(self.a == 0);
            },
            else => {}, // TODO: brk, rti
        }
    }

    /// For debug printing
    pub fn printState(self: *Cpu, inst: Inst, opts: RunOptions) void {
        // PC
        print("{x:0<4}: ", .{self.pc - inst.size()});

        // Op
        if (opts.print == .yes_color)
            print("\x1b[33m{}\x1b[0m ", .{inst.op.name()})
        else
            print("{} ", .{inst.op.name()});

        // Addressing mode and operand
        const operand = inst.operand;
        {
            var buf: [12]u8 = undefined;
            var fbs = std.io.fixedBufferStream(&buf);
            const writer = fbs.writer();
            const ok = switch (inst.addr_mode) {
                .abs => writer.print("${x:0<4}", .{operand.addr}),
                .abs_x => writer.print("${x:0<4},X", .{operand.addr}),
                .abs_y => writer.print("${x:0<4},Y", .{operand.addr}),
                .acc => {},
                .imm => writer.print("#${x:0<2}", .{operand.imm}),
                .impl => {},
                .ind => writer.print("(${x:0<4})", .{operand.addr}),
                .ind_y => writer.print("(${x:0<4}),Y", .{operand.addr}),
                .rel => writer.print("${x:0<2}", .{operand.rel}),
                .x_ind => writer.print("(${x:0<4},X)", .{operand.addr}),
                .zp => writer.print("${x:0<2}", .{operand.addr}),
                .zp_x => writer.print("${x:0<2},X", .{operand.addr}),
                .zp_y => writer.print("${x:0<2},Y", .{operand.addr}),
            };
            ok catch unreachable;
            if (opts.print == .yes_color)
                print("\x1b[34m{: <10}\x1b[0m", .{fbs.getWritten()})
            else
                print("{: <10}", .{fbs.getWritten()});
        }

        // Dereference address
        switch (inst.operand) {
            .addr => {
                const addr = self.resolveAddress(inst);
                print("[ #${x:0<2} ]", .{self.memory[addr]});
            },
            else => print("        ", .{}),
        }

        // Registers
        print(" | ", .{});
        if (opts.print == .yes_color) {
            print("{}A{}: {x:0<2} | ", .{ "\x1b[32m", "\x1b[0m", self.a });
            print("{}X{}: {x:0<2} | ", .{ "\x1b[32m", "\x1b[0m", self.x });
            print("{}Y{}: {x:0<2} | ", .{ "\x1b[32m", "\x1b[0m", self.y });
            print("{}SP{}: {x:0<2} | ", .{ "\x1b[32m", "\x1b[0m", self.sp });
        } else {
            print("A: {x:0<2} | X: {x:0<2} | Y: {x:0<2} | SP: {x:0<2} | ", .{
                self.a,
                self.x,
                self.y,
                self.sp,
            });
        }

        // Status register
        if (self.sr.negative == 1) print("N", .{}) else print("-", .{});
        if (self.sr.overflow == 1) print("V", .{}) else print("-", .{});
        if (self.sr.zero == 1) print("Z", .{}) else print("-", .{});
        if (self.sr.carry == 1) print("C", .{}) else print("-", .{});

        print(" | ", .{});

        // TODO: pretty-print the stack somehow

        print("\n", .{});
    }

    fn read(self: Cpu, comptime T: type, addr: u16) T {
        const size = @sizeOf(T);
        return mem.readIntSliceLittle(T, self.memory[addr .. addr + size]);
    }

    fn resolveAddress(self: Cpu, inst: Inst) u16 {
        const addr = inst.operand.addr;
        return switch (inst.addr_mode) {
            .abs, .zp => addr,
            .abs_x => self.x + addr,
            .abs_y => self.y + addr,
            .ind => self.read(u16, addr),
            .x_ind => self.read(u16, addr +% self.x),
            .ind_y => self.y + self.read(u16, addr),
            .zp_x => self.x +% addr,
            .zp_y => self.y +% addr,
            else => unreachable,
        };
    }

    fn push(self: *Cpu, comptime T: type, value: T) void {
        const sp = stack_bottom + @intCast(u16, self.sp);
        const size = @sizeOf(T);
        // writeIntSlice can't handle u8 apparently
        mem.writeIntSliceLittle(u16, self.memory[sp - 1 .. sp + (size - 1)], value);
        self.sp -%= size;
    }

    fn pull(self: *Cpu, comptime T: type) T {
        const sp = stack_bottom + @intCast(u16, self.sp);
        const size = @sizeOf(T);
        defer self.sp +%= size;
        return mem.readIntSliceLittle(T, self.memory[sp + 1 .. sp + 1 + size]);
    }

    fn jumpBy(self: *Cpu, delta: u8) void {
        if (delta & 0x80 == 0)
            self.pc += delta
        else
            self.pc -= ~delta +% 1;
    }

    fn carry(self: *Cpu) u1 {
        if (self.sr.carry == 1) {
            self.sr.carry = 0;
            return 1;
        }
        return 0;
    }

    const RunOptions = struct {
        print: PrintOptions = .no,

        const PrintOptions = enum { no, yes, yes_color };
    };

    const Error = error{
        InvalidOpcode,
        StackUnderflowReturnFromSubroutine,
    };
};

pub fn main() anyerror!void {
    const entry = 0x8000;

    const file = try std.fs.cwd().openFile("tests/day.bin", .{ .read = true });
    var memory: [0x10000]u8 = undefined;
    _ = try file.read(memory[entry..]);

    var cpu = Cpu.init(&memory, entry);
    try cpu.run(.{ .print = .yes });
}
