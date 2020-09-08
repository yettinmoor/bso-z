const std = @import("std");
const mem = std.mem;
const print = std.debug.print;

const insts = @import("insts.zig");

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

    pub fn run(self: *Cpu) !void {
        // TEMP
        while (true) {
            // For debug printing below
            const pc = self.pc;

            const opcode = self.next(u8);
            const op = insts.op_table[opcode] orelse return error.InvalidOpcode;
            const addr_mode = insts.addr_mode_table[opcode] orelse return error.InvalidOpcode;

            const operand: union(enum) {
                imm: u8,
                addr: u16,
                impl,
            } = switch (addr_mode) {
                // addressing modes:
                //    immediate: imm, rel
                //    addressing: abs(x|y), ind(x|y), zp(x|y)
                //    implied: acc, impl
                .acc => .impl,
                .abs => .{ .addr = self.next(u16) },
                .abs_x => .{ .addr = self.x + self.next(u16) },
                .abs_y => .{ .addr = self.y + self.next(u16) },
                .imm => .{ .imm = self.next(u8) },
                .impl => .impl,
                .ind => blk: {
                    const ind_addr = self.next(u16);
                    break :blk .{ .addr = mem.readIntSliceLittle(u16, self.memory[ind_addr .. ind_addr + 1]) };
                },
                .x_ind => blk: {
                    const ind_addr = self.x +% self.next(u8);
                    break :blk .{ .addr = mem.readIntSliceLittle(u16, self.memory[ind_addr .. ind_addr + 1]) };
                },
                .ind_y => blk: {
                    const ind_addr = self.next(u8);
                    break :blk .{ .addr = self.y + mem.readIntSliceLittle(u16, self.memory[ind_addr .. ind_addr + 1]) };
                },
                .rel => .{ .imm = self.next(u8) },
                .zp => .{ .addr = self.next(u8) },
                .zp_x => .{ .addr = self.next(u8) +% self.x },
                .zp_y => .{ .addr = self.next(u8) +% self.y },
            };

            // to print if instruction was handled
            var have_op = true;

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
                .bcc => if (self.sr.carry == 0) self.jumpBy(operand.imm),
                .bcs => if (self.sr.carry == 1) self.jumpBy(operand.imm),
                .beq => if (self.sr.zero == 1) self.jumpBy(operand.imm),
                // .bit,
                // .bmi,
                // .bne,
                // .bpl,
                .brk => break,
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
                    };
                    self.sr.negative = @boolToInt(self.a & 0x80 != 0);
                    self.sr.zero = @boolToInt(self.a == 0);
                },
                .ldx => {
                    self.x = switch (operand) {
                        .imm => |imm| imm,
                        .addr => |addr| self.memory[addr],
                        .impl => 1,
                    };
                    self.sr.negative = @boolToInt(self.x & 0x80 != 0);
                    self.sr.zero = @boolToInt(self.x == 0);
                },
                .ldy => {
                    self.y = switch (operand) {
                        .imm => |imm| imm,
                        .addr => |addr| self.memory[addr],
                        .impl => 1,
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
                // .nop,
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
                    if (self.sp == 0xff) break;
                    const sp = stack_bottom + @intCast(u16, self.sp);
                    const ret_addr = mem.readIntSliceLittle(u16, self.memory[sp + 1 .. sp + 3]);
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
                else => have_op = false,
            }

            // Debug printing
            {
                if (opcode != 0xaa and opcode != 0) {
                    print("0x{x:0<4}: ", .{pc});
                    if (have_op)
                        print("[✓] ", .{})
                    else
                        print("[ ] ", .{});

                    switch (operand) {
                        .imm => |imm| print("{} #${x:0<2}   ", .{
                            // @tagName(op),
                            insts.op_name_table[opcode],
                            imm,
                        }),
                        .addr => |addr| print("{} ${x:0<4}  ", .{
                            insts.op_name_table[opcode],
                            addr,
                        }),
                        .impl => print("{}        ", .{
                            insts.op_name_table[opcode],
                        }),
                    }

                    print("A: 0x{x:0<2} | X: 0x{x:0<2} | Y: 0x{x:0<2} | ", .{
                        self.a,
                        self.x,
                        self.y,
                    });

                    if (self.sr.negative == 1) print("N", .{}) else print("-", .{});
                    if (self.sr.overflow == 1) print("V", .{}) else print("-", .{});
                    if (self.sr.zero == 1) print("Z", .{}) else print("-", .{});
                    if (self.sr.carry == 1) print("C", .{}) else print("-", .{});

                    switch (operand) {
                        .addr => |addr| print("  ${x:0<4}: #${x:0<2}", .{
                            addr,
                            self.memory[addr],
                        }),
                        else => {},
                    }
                    print("\n", .{});

                    // if (op == .jsr or op == .rts) {
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
            }
        }
    }

    fn next(self: *Cpu, comptime T: type) T {
        const size = @sizeOf(T);
        defer self.pc += size;
        return mem.readIntSliceLittle(T, self.memory[self.pc .. self.pc + size]);
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
};

pub fn main() anyerror!void {
    const entry = 0x8000;

    const file = try std.fs.cwd().openFile("tests/day.bin", .{ .read = true });
    var memory: [0x10000]u8 = undefined;
    _ = try file.read(memory[entry..]);

    var cpu = Cpu.init(&memory, entry);
    try cpu.run();
}
