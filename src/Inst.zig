//! https://www.masswerk.at/6502/6502_instruction_set.html

op: Op,
addr_mode: AddressingMode,
operand: Operand,

const readIntSliceLittle = @import("std").mem.readIntSliceLittle;
const Inst = @This();

pub const Operand = union(enum) {
    imm: u8,
    rel: u8,
    addr: u16,
    impl,
};

pub inline fn size(inst: Inst) u8 {
    return 1 + switch (inst.addr_mode) {
        .acc, .impl => @as(u8, 0),
        .imm, .rel, .zp, .zp_x, .zp_y => @as(u8, 1),
        .abs, .abs_x, .abs_y, .ind, .x_ind, .ind_y => @as(u8, 2),
    };
}

/// Accepts slice beginning at the opcode to be disassembled.
pub inline fn fetch(memory: []const u8) !Inst {
    const opcode = memory[0];
    const op = op_table[opcode] orelse return error.InvalidOpcode;
    const addr_mode = addr_mode_table[opcode] orelse return error.InvalidOpcode;
    const operand: Operand = switch (addr_mode) {
        .acc, .impl => .impl,
        .imm => .{ .imm = readIntSliceLittle(u8, memory[1..]) },
        .rel => .{ .rel = readIntSliceLittle(u8, memory[1..]) },

        .abs,
        .abs_x,
        .abs_y,
        .ind,
        .x_ind,
        .ind_y,
        => .{ .addr = readIntSliceLittle(u16, memory[1..]) },

        .zp,
        .zp_x,
        .zp_y,
        => .{ .addr = readIntSliceLittle(u8, memory[1..]) },
    };
    return Inst{
        .op = op,
        .addr_mode = addr_mode,
        .operand = operand,
    };
}

const Op = enum {
    adc,
    @"and",
    asl,
    bcc,
    bcs,
    beq,
    bit,
    bmi,
    bne,
    bpl,
    brk,
    bvc,
    bvs,
    clc,
    cld,
    cli,
    clv,
    cmp,
    cpx,
    cpy,
    dec,
    dex,
    dey,
    eor,
    inc,
    inx,
    iny,
    jmp,
    jsr,
    lda,
    ldx,
    ldy,
    lsr,
    nop,
    ora,
    pha,
    php,
    pla,
    plp,
    rol,
    ror,
    rti,
    rts,
    sbc,
    sec,
    sed,
    sei,
    sta,
    stx,
    sty,
    tax,
    tsx,
    txa,
    txs,
    tya,

    pub fn name(op: Op) [3]u8 {
        const tag_name = @tagName(op);
        return [3]u8{ tag_name[0] - 0x20, tag_name[1] - 0x20, tag_name[2] - 0x20 };
    }
};

const AddressingMode = enum {
    /// accumulator: OPC A: operand is AC (implied single byte instruction)
    acc,
    /// absolute: OPC $LLHH: operand is address $HHLL *
    abs,
    /// absolute, X-indexed: OPC $LLHH,X: operand is address; effective address is address incremented by X with carry **
    abs_x,
    /// absolute, Y-indexed: OPC $LLHH,Y: operand is address; effective address is address incremented by Y with carry **
    abs_y,
    /// immediate: OPC #$BB: operand is byte BB
    imm,
    /// implied: OPC: operand implied
    impl,
    /// indirect: OPC ($LLHH): operand is address; effective address is contents of word at address: C.w($HHLL)
    ind,
    /// X-indexed, indirect: OPC ($LL,X): operand is zeropage address; effective address is word in (LL + X, LL + X + 1), inc. without carry: C.w($00LL + X)
    x_ind,
    /// indirect, Y-indexed: OPC ($LL),Y: operand is zeropage address; effective address is word in (LL, LL + 1) incremented by Y with carry: C.w($00LL) + Y
    ind_y,
    /// relative: OPC $BB: branch target is PC + signed offset BB ***
    rel,
    /// zeropage: OPC $LL: operand is zeropage address (hi-byte is zero, address = $00LL)
    zp,
    /// zeropage, X-indexed: OPC $LL,X: operand is zeropage address; effective address is address incremented by X without carry **
    zp_x,
    /// zeropage, Y-indexed: OPC $LL,Y: operand is zeropage address; effective address is address incremented by Y without carry **
    zp_y,
};

const op_table = [256]?Op{
    //00    01       02    03    04    05       06    07    08    09       0a    0b    0c    0d       0e    0f
    .brk, .ora,    null, null, null, .ora,    .asl, null, .php, .ora,    .asl, null, null, .ora,    .asl, null, // 00
    .bpl, .ora,    null, null, null, .ora,    .asl, null, .clc, .ora,    null, null, null, .ora,    .asl, null, // 10
    .jsr, .@"and", null, null, .bit, .@"and", .rol, null, .plp, .@"and", .rol, null, .bit, .@"and", .rol, null, // 20
    .bmi, .@"and", null, null, null, .@"and", .rol, null, .sec, .@"and", null, null, null, .@"and", .rol, null, // 30
    .rti, .eor,    null, null, null, .eor,    .lsr, null, .pha, .eor,    .lsr, null, .jmp, .eor,    .lsr, null, // 40
    .bvc, .eor,    null, null, null, .eor,    .lsr, null, .cli, .eor,    null, null, null, .eor,    .lsr, null, // 50
    .rts, .adc,    null, null, null, .adc,    .ror, null, .pla, .adc,    .ror, null, .jmp, .adc,    .ror, null, // 60
    .bvs, .adc,    null, null, null, .adc,    .ror, null, .sei, .adc,    null, null, null, .adc,    .ror, null, // 70
    null, .sta,    null, null, .sty, .sta,    .stx, null, .dey, null,    .txa, null, .sty, .sta,    .stx, null, // 80
    .bcc, .sta,    null, null, .sty, .sta,    .stx, null, .tya, .sta,    .txs, null, null, .sta,    null, null, // 90
    .ldy, .lda,    .ldx, null, .ldy, .lda,    .ldx, null, .tax, .lda,    .tax, null, .ldy, .lda,    .ldx, null, // a0
    .bcs, .lda,    null, null, .ldy, .lda,    .ldx, null, .clv, .lda,    .tsx, null, .ldy, .lda,    .ldx, null, // b0
    .cpy, .cmp,    null, null, .cpy, .cmp,    .dec, null, .iny, .cmp,    .dex, null, .cpy, .cmp,    .dec, null, // c0
    .bne, .cmp,    null, null, null, .cmp,    .dec, null, .cld, .cmp,    null, null, null, .cmp,    .dec, null, // d0
    .cpx, .sbc,    null, null, .cpx, .sbc,    .inc, null, .inx, .sbc,    .nop, null, .cpx, .sbc,    .inc, null, // e0
    .beq, .sbc,    null, null, null, .sbc,    .inc, null, .sed, .sbc,    null, null, null, .sbc,    .inc, null, // f0
};

const addr_mode_table = [256]?AddressingMode{
    //00    01        02   03    04    05     06      07     08    09      0a     0b    0c      0d      0e      0f
    .impl, .x_ind, null, null, null,  .zp,   .zp,   null, .impl, .imm,   .acc,  null, null,   .abs,   .abs,   null, // 00
    .rel,  .ind_y, null, null, null,  .zp_x, .zp_x, null, .impl, .abs_y, null,  null, null,   .abs_x, .abs_x, null, // 10
    .abs,  .x_ind, null, null, .zp,   .zp,   .zp,   null, .impl, .imm,   .acc,  null, .abs,   .abs,   .abs,   null, // 20
    .rel,  .ind_y, null, null, null,  .zp_x, .zp_x, null, .impl, .abs_y, null,  null, null,   .abs_x, .abs_x, null, // 30
    .impl, .x_ind, null, null, null,  .zp,   .zp,   null, .impl, .imm,   .acc,  null, .abs,   .abs,   .abs,   null, // 40
    .rel,  .ind_y, null, null, null,  .zp_x, .zp_x, null, .impl, .abs_y, null,  null, null,   .abs_x, .abs_x, null, // 50
    .impl, .x_ind, null, null, null,  .zp,   .zp,   null, .impl, .imm,   .acc,  null, .ind,   .abs,   .abs,   null, // 60
    .rel,  .ind_y, null, null, null,  .zp_x, .zp_x, null, .impl, .abs_y, null,  null, null,   .abs_x, .abs_x, null, // 70
    null,  .x_ind, null, null, .zp,   .zp,   .zp,   null, .impl, null,   .impl, null, .abs,   .abs,   .abs,   null, // 80
    .rel,  .ind_y, null, null, .zp_x, .zp_x, .zp_y, null, .impl, .abs_y, .impl, null, null,   .abs_x, null,   null, // 90
    .imm,  .x_ind, .imm, null, .zp,   .zp,   .zp,   null, .impl, .imm,   .impl, null, .abs,   .abs,   .abs,   null, // a0
    .rel,  .ind_y, null, null, .zp_x, .zp_x, .zp_y, null, .impl, .abs_y, .impl, null, .abs_x, .abs_x, .abs_y, null, // b0
    .imm,  .x_ind, null, null, .zp,   .zp,   .zp,   null, .impl, .imm,   .impl, null, .abs,   .abs,   .abs,   null, // c0
    .rel,  .ind_y, null, null, null,  .zp_x, .zp_x, null, .impl, .abs_y, null,  null, null,   .abs_x, .abs_x, null, // d0
    .imm,  .x_ind, null, null, .zp,   .zp,   .zp,   null, .impl, .imm,   .impl, null, .abs,   .abs,   .abs,   null, // e0
    .rel,  .ind_y, null, null, null,  .zp_x, .zp_x, null, .impl, .abs_y, null,  null, null,   .abs_x, .abs_x, null, // f0
};
