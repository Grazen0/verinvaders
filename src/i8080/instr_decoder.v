`default_nettype none `timescale 1ns / 1ps

`include "i8080.vh"

module instr_decoder #(
    parameter XLEN = 8
) (
    input wire [XLEN-1:0] instr,
    input wire [XLEN-1:0] flags,

    output wire [2:0] sss,
    output wire [2:0] ddd,
    output wire [1:0] rp,
    output wire [2:0] alu_op,

    output wire is_sss_mem,
    output wire is_sss_a,
    output wire is_ddd_mem,
    output wire is_ddd_a,
    output wire is_alu_op_cmp,
    output wire is_rp_psw,

    output reg is_mov,
    output reg is_sphl,
    output reg is_mvi,
    output reg is_lxi,
    output reg is_lda,
    output reg is_sta,
    output reg is_lhld,
    output reg is_shld,
    output reg is_ldax,
    output reg is_stax,
    output reg is_xchg,
    output reg is_alu_reg,
    output reg is_alu_imm,
    output reg is_alu_alt,
    output reg is_inr,
    output reg is_dcr,
    output reg is_inx,
    output reg is_dcx,
    output reg is_dad,
    output reg is_jmp,
    output reg is_call,
    output reg is_ret,
    output reg is_rst,
    output reg is_pchl,
    output reg is_push,
    output reg is_pop,
    output reg is_xthl,
    output reg is_in,
    output reg is_out,
    output reg is_ei,
    output reg is_di,
    output reg is_hlt,
    output reg is_nop,

    output wire use_branch_cond,
    output reg  branch_cond
);
  localparam REG_MEM = 3'b110;
  localparam REG_A = 3'b111;

  wire [2:0] cc = instr[5:3];

  assign use_branch_cond = ~instr[0];

  always @(*) begin
    case (cc)
      3'b000:  branch_cond = ~flags[`FZ];
      3'b001:  branch_cond = flags[`FZ];
      3'b010:  branch_cond = ~flags[`FC];
      3'b011:  branch_cond = flags[`FC];
      3'b100:  branch_cond = ~flags[`FP];
      3'b101:  branch_cond = flags[`FP];
      3'b110:  branch_cond = ~flags[`FS];
      3'b111:  branch_cond = flags[`FS];
      default: branch_cond = 1'bx;
    endcase

    branch_cond = branch_cond | instr[0];

    is_mov      = 0;
    is_sphl     = 0;
    is_mvi      = 0;
    is_lxi      = 0;
    is_lda      = 0;
    is_sta      = 0;
    is_lhld     = 0;
    is_shld     = 0;
    is_ldax     = 0;
    is_stax     = 0;
    is_xchg     = 0;
    is_alu_reg  = 0;
    is_alu_imm  = 0;
    is_alu_alt  = 0;
    is_inr      = 0;
    is_dcr      = 0;
    is_inx      = 0;
    is_dcx      = 0;
    is_dad      = 0;
    is_jmp      = 0;
    is_call     = 0;
    is_ret      = 0;
    is_rst      = 0;
    is_pchl     = 0;
    is_push     = 0;
    is_pop      = 0;
    is_xthl     = 0;
    is_in       = 0;
    is_out      = 0;
    is_ei       = 0;
    is_di       = 0;
    is_hlt      = 0;
    is_nop      = 0;

    casez (instr)
      8'b01_110_110: is_hlt = 1;
      8'b01_zzz_zzz: is_mov = 1;
      8'b11_111_001: is_sphl = 1;
      8'b00_zzz_110: is_mvi = 1;
      8'b00_zz0_001: is_lxi = 1;
      8'b00_111_010: is_lda = 1;
      8'b00_110_010: is_sta = 1;
      8'b00_101_010: is_lhld = 1;
      8'b00_100_010: is_shld = 1;
      8'b00_zz1_010: is_ldax = 1;
      8'b00_zz0_010: is_stax = 1;
      8'b11_101_011: is_xchg = 1;
      8'b10_zzz_zzz: is_alu_reg = 1;
      8'b11_zzz_110: is_alu_imm = 1;
      8'b00_zzz_111: is_alu_alt = 1;
      8'b00_zzz_100: is_inr = 1;
      8'b00_zzz_101: is_dcr = 1;
      8'b00_zz0_011: is_inx = 1;
      8'b00_zz1_011: is_dcx = 1;
      8'b00_zz1_001: is_dad = 1;
      8'b11_000_011: is_jmp = 1;
      8'b11_zzz_010: is_jmp = 1;
      8'b11_001_101: is_call = 1;
      8'b11_zzz_100: is_call = 1;
      8'b11_001_001: is_ret = 1;
      8'b11_zzz_000: is_ret = 1;
      8'b11_zzz_111: is_rst = 1;
      8'b11_101_001: is_pchl = 1;
      8'b11_zz0_101: is_push = 1;
      8'b11_zz0_001: is_pop = 1;
      8'b11_100_011: is_xthl = 1;
      8'b11_011_011: is_in = 1;
      8'b11_010_011: is_out = 1;
      8'b11_111_011: is_ei = 1;
      8'b11_110_011: is_di = 1;
      default:       is_nop = 1;
    endcase
  end

  assign sss = instr[2:0];
  assign ddd = instr[5:3];
  assign rp = instr[5:4];
  assign alu_op = instr[5:3];

  assign is_sss_mem = sss == REG_MEM;
  assign is_sss_a = sss == REG_A;
  assign is_ddd_mem = ddd == REG_MEM;
  assign is_ddd_a = ddd == REG_A;
  assign is_alu_op_cmp = alu_op == 3'b111;
  assign is_rp_psw = rp == 2'b11;
endmodule

