const std = @import("std");
const mem = std.mem;
const assert = std.debug.assert;
const readInt = mem.readIntSliceLittle;
const print = std.debug.print;

const Inst = @import("Inst.zig");

const Cpu = struct {
    pc: u16,
    a: u8,
    x: u8,
    y: u8,
    sp: u8,
    sr: struct { // NVssDIZC
        carry: u1,
        zero: u1,
        interrupt_disable: u1,
        decimal: u1,
        @"break": u1,
        overflow: u1,
        negative: u1,
    },

    memory: []u8,
    entry_point: u16,

    const stack_top = 0x0200;
    const stack_bottom = 0x0100;

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
        self.sr = .{
            .carry = 0,
            .zero = 0,
            .interrupt_disable = 0,
            .decimal = 0,
            .@"break" = 0,
            .overflow = 0,
            .negative = 0,
        };
    }

    pub fn run(self: *Cpu) Error!void {
        while (true) {
            const inst = try self.fetch();
            self.printState(inst);
            self.step(inst) catch |err| switch (err) {
                // TODO: for now, we exit when returning from a subroutine
                // when no return address is on the stack
                Error.StackUnderflowReturnFromSubroutine => return,
                Error.InvalidOpcode => return err,
            };
        }
    }

    /// Fetch the next instruction, advancing the program counter
    pub fn fetch(self: *Cpu) Error!Inst {
        const pc = self.pc;
        const opcode = self.next(u8);
        var inst = try Inst.new(opcode);
        inst.operand = switch (inst.addr_mode) {
            .acc, .impl => .impl,
            .imm => .{ .imm = self.next(u8) },
            .rel => .{ .rel = self.next(u8) },

            .abs,
            .abs_x,
            .abs_y,
            .ind,
            .x_ind,
            .ind_y,
            => .{ .addr = self.next(u16) },

            .zp,
            .zp_x,
            .zp_y,
            => .{ .addr = self.next(u8) },
        };
        assert(self.pc == pc + inst.len);
        return inst;
    }

    /// Run ONE instruction
    pub fn step(self: *Cpu, inst: Inst) Error!void {
        const op = inst.op;

        // Addresses must be recalculated in the case of certain addressing modes.
        // We don't do this when fetching in order to preserve the original disassembly
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
                self.a |= val;
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
            // .bit,
            // .bmi,
            // .bne,
            // .bpl,
            // .brk,
            // .bvc,
            // .bvs,
            .clc => self.sr.carry = 0,
            // .cld,
            // .cli,
            // .clv,
            // reg < memory => N
            // reg = memory => ZC
            // reg > memory => C
            .cmp, .cpx, .cpy => {
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
                self.sr.negative = @boolToInt(self.y & 0x80 != 0);
                self.sr.zero = @boolToInt(self.y == 0);
            },
            // .inc,
            // .inx,
            // .iny,
            .jmp => self.pc = operand.addr,
            .jsr => {
                const sp = stack_bottom + @intCast(u16, self.sp);
                mem.writeIntSliceLittle(
                    u16,
                    self.memory[sp - 1 .. sp + 1],
                    self.pc,
                );
                self.sp -%= 2;
                self.pc = operand.addr;
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
            // .ora,
            // .pha,
            // .php,
            // .pla,
            // .plp,
            // .rol,
            .ror => {
                const target = switch (operand) {
                    .addr => |addr| &self.memory[addr],
                    .impl => &self.a,
                    else => unreachable,
                };
                const is_carry = @truncate(u1, target.*);
                target.* = (target.* >> 1);
                if (self.carry() == 1) target.* |= 0x80;
                self.sr.carry = is_carry;
                self.sr.negative = @boolToInt(target.* & 0x80 != 0);
                self.sr.zero = @boolToInt(target.* == 0);
            },
            // .rti,
            .rts => {
                if (self.sp == 0xff)
                    return error.StackUnderflowReturnFromSubroutine;
                const sp = stack_bottom + @intCast(u16, self.sp);
                const ret_addr = readInt(u16, self.memory[sp + 1 .. sp + 3]);
                self.sp +%= 2;
                self.pc = ret_addr;
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
            // .sec,
            // .sed,
            // .sei,
            .sta => {
                self.memory[operand.addr] = self.a;
            },
            .stx => {
                self.memory[operand.addr] = self.x;
            },
            .sty => {
                self.memory[operand.addr] = self.y;
            },
            .tax => {
                self.x = self.a;
                self.sr.negative = @boolToInt(self.x & 0x80 != 0);
                self.sr.zero = @boolToInt(self.x == 0);
            },
            // .tsx,
            .txa => {
                self.a = self.x;
                self.sr.negative = @boolToInt(self.a & 0x80 != 0);
                self.sr.zero = @boolToInt(self.a == 0);
            },
            // .txs,
            .tya => {
                self.a = self.y;
                self.sr.negative = @boolToInt(self.a & 0x80 != 0);
                self.sr.zero = @boolToInt(self.a == 0);
            },
            else => print("TODO ", .{}),
        }
    }

    /// For debug printing
    pub fn printState(self: *Cpu, inst: Inst) void {
        // PC
        print("  0x{x:0<4}:  ", .{self.pc - inst.len});

        // Op
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

        // Register status
        print("  | A: 0x{x:0<2} | X: 0x{x:0<2} | Y: 0x{x:0<2} | SP: 0x{x:0<2} | ", .{
            self.a,
            self.x,
            self.y,
            self.sp,
        });

        if (self.sr.negative == 1) print("N", .{}) else print("-", .{});
        if (self.sr.overflow == 1) print("V", .{}) else print("-", .{});
        if (self.sr.zero == 1) print("Z", .{}) else print("-", .{});
        if (self.sr.carry == 1) print("C", .{}) else print("-", .{});

        print(" | ", .{});

        print("\n", .{});

        // if (inst.op == .jsr or inst.op == .rts) {
        //     print("\t[ ", .{});
        //     for (self.memory[0x1f0..0x200]) |s, i| {
        //         if (i + 0xf0 == self.sp)
        //             print("<{x:0<2}> ", .{s})
        //         else
        //             print("{x:0<2} ", .{s});
        //     }
        //     print("]\n", .{});
        // }
    }

    fn next(self: *Cpu, comptime T: type) T {
        const size = @sizeOf(T);
        defer self.pc += size;
        return readInt(T, self.memory[self.pc .. self.pc + size]);
    }

    fn resolveAddress(self: Cpu, inst: Inst) u16 {
        const addr = inst.operand.addr;
        return switch (inst.addr_mode) {
            .abs, .zp => addr,
            .abs_x => self.x + addr,
            .abs_y => self.y + addr,
            .ind => readInt(u16, self.memory[addr .. addr + 1]),
            .x_ind => readInt(u16, self.memory[(addr +% self.x) .. addr +% self.x +% 1]),
            .ind_y => self.y + readInt(u16, self.memory[addr .. addr + 1]),
            .zp_x => self.x +% addr,
            .zp_y => self.y +% addr,
            else => unreachable,
        };
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
    try cpu.run();
}
