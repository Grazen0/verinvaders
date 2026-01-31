`default_nettype none `timescale 1ns / 1ps

`include "i8080.vh"

module i8080 #(
    parameter XLEN = 8
) (
    input wire clk,
    input wire rst,

    inout  tri  [  XLEN-1:0] data,
    output wire [2*XLEN-1:0] addr,

    input  wire ready,
    output wire wwait,

    input  wire iint,
    output wire inte,

    output wire sync,

    output wire dbin,
    output wire write_n
);
  localparam ZZ = {XLEN{1'bz}};

  tri  [XLEN-1:0] bus;

  wire [XLEN-1:0] flags;
  wire [XLEN-1:0] a;
  wire [XLEN-1:0] act;
  wire [XLEN-1:0] tmp;
  wire [XLEN-1:0] instr;
  wire [XLEN-1:0] alu_flags_out;
  wire [XLEN-1:0] alu_out;
  wire [XLEN-1:0] regs_out;

  wire read_flags, write_flags;
  wire read_a, write_a;
  wire write_act;
  wire read_tmp, write_tmp;
  wire write_instr;
  wire read_alu;
  wire read_regs, write_regs;

  assign bus = read_a ? a : ZZ;
  assign bus = read_tmp ? tmp : ZZ;
  assign bus = read_flags ? flags : ZZ;
  assign bus = read_alu ? alu_out : ZZ;
  assign bus = read_regs ? regs_out : ZZ;

  register #(
      .WIDTH(XLEN),
      .RESET_VALUE(8'b0000_0010)
  ) flags_reg (
      .clk  (clk),
      .rst(rst),

      .wenable(write_flags),

      .in (flags_src == `FLAGS_SRC_ALU ? alu_flags_out : bus),
      .out(flags)
  );

  register #(
      .WIDTH(XLEN)
  ) a_reg (
      .clk  (clk),
      .rst(rst),

      .wenable(write_a),

      .in (a_src == `A_SRC_BUS ? bus : alu_out),
      .out(a)
  );

  register #(
      .WIDTH(XLEN)
  ) act_reg (
      .clk  (clk),
      .rst(rst),

      .wenable(write_act),
      .in     (act_src == `ACT_SRC_A ? a : bus),
      .out    (act)
  );

  register #(
      .WIDTH(XLEN)
  ) tmp_reg (
      .clk  (clk),
      .rst(rst),

      .wenable(write_tmp),
      .in     (bus),
      .out    (tmp)
  );

  register #(
      .WIDTH(XLEN)
  ) instr_reg (
      .clk  (clk),
      .rst(rst),

      .wenable(write_instr),
      .in     (bus),
      .out    (instr)
  );

  alu #(
      .XLEN(XLEN)
  ) alu (
      .op_a   (act),
      .op_b   (tmp),
      .control(alu_control),

      .flags_in (flags),
      .flags_out(alu_flags_out),

      .out(alu_out)
  );

  wire [2*XLEN-1:0] regs_out_rp;

  register_array #(
      .XLEN(XLEN)
  ) reg_array (
      .clk  (clk),
      .rst(rst),

      .reg_sel(reg_sel),
      .wdata  (bus),
      .wenable(write_regs),
      .rdata  (regs_out),
      .rpdata (regs_out_rp),

      .swap_hl_de       (swap_hl_de),
      .cpy_hl_to_sp     (cpy_hl_to_sp),
      .cpy_hl_to_pc     (cpy_hl_to_pc),
      .cpy_wz_to_hl     (cpy_wz_to_hl),
      .cpy_wz_next_to_pc(cpy_wz_next_to_pc),
      .instr            (instr),
      .write_wz_dup     (write_wz_dup),
      .write_wz_rst     (write_wz_rst),

      .inc(inc_rp),
      .dec(dec_rp)
  );

  wire is_sss_mem, is_sss_a, is_ddd_mem, is_ddd_a, is_alu_op_cmp, is_rp_psw;
  wire [2:0] sss, ddd, alu_op;
  wire [1:0] rp;
  wire is_mov, is_sphl, is_mvi, is_lxi, is_lda, is_sta, is_lhld, is_shld, is_ldax, is_stax, is_xchg,
       is_alu_reg, is_alu_imm, is_alu_alt, is_inr, is_dcr, is_inx, is_dcx, is_dad, is_jmp, is_pchl,
       is_call, is_ret, is_rst, is_push, is_pop, is_xthl, is_in, is_out, is_ei, is_di, is_hlt,
       is_nop;
  wire use_branch_cond, branch_cond;

  instr_decoder #(
      .XLEN(XLEN)
  ) instr_decoder (
      .instr(instr),
      .flags(flags),

      .is_sss_mem   (is_sss_mem),
      .is_sss_a     (is_sss_a),
      .is_ddd_mem   (is_ddd_mem),
      .is_ddd_a     (is_ddd_a),
      .is_alu_op_cmp(is_alu_op_cmp),
      .is_rp_psw    (is_rp_psw),

      .sss   (sss),
      .ddd   (ddd),
      .rp    (rp),
      .alu_op(alu_op),

      .is_mov    (is_mov),
      .is_sphl   (is_sphl),
      .is_mvi    (is_mvi),
      .is_lxi    (is_lxi),
      .is_lda    (is_lda),
      .is_sta    (is_sta),
      .is_lhld   (is_lhld),
      .is_shld   (is_shld),
      .is_ldax   (is_ldax),
      .is_stax   (is_stax),
      .is_xchg   (is_xchg),
      .is_alu_reg(is_alu_reg),
      .is_alu_imm(is_alu_imm),
      .is_alu_alt(is_alu_alt),
      .is_inr    (is_inr),
      .is_dcr    (is_dcr),
      .is_inx    (is_inx),
      .is_dcx    (is_dcx),
      .is_dad    (is_dad),
      .is_jmp    (is_jmp),
      .is_call   (is_call),
      .is_ret    (is_ret),
      .is_rst    (is_rst),
      .is_pchl   (is_pchl),
      .is_push   (is_push),
      .is_pop    (is_pop),
      .is_xthl   (is_xthl),
      .is_in     (is_in),
      .is_out    (is_out),
      .is_ei     (is_ei),
      .is_di     (is_di),
      .is_hlt    (is_hlt),
      .is_nop    (is_nop),

      .use_branch_cond(use_branch_cond),
      .branch_cond    (branch_cond)
  );

  wire int_sync;

  synchronizer int_synchronizer (
      .clk  (clk),
      .rst(rst),

      .in (iint),
      .out(int_sync)
  );


  wire       write_adr;
  wire [3:0] reg_sel;
  wire [4:0] alu_control;
  wire       write;
  wire a_src, act_src, flags_src;

  wire data_in_enable, data_out_enable;
  wire write_data_out;
  wire swap_hl_de, cpy_hl_to_sp, cpy_hl_to_pc, cpy_wz_to_hl, cpy_wz_next_to_pc,
    write_wz_dup, write_wz_rst;

  wire inc_rp, dec_rp;
  wire [XLEN-1:0] status;

  control #(
      .XLEN(XLEN)
  ) control (
      .clk  (clk),
      .rst(rst),

      .sss   (sss),
      .ddd   (ddd),
      .rp    (rp),
      .alu_op(alu_op),

      .is_sss_mem   (is_sss_mem),
      .is_sss_a     (is_sss_a),
      .is_ddd_mem   (is_ddd_mem),
      .is_ddd_a     (is_ddd_a),
      .is_alu_op_cmp(is_alu_op_cmp),
      .is_rp_psw    (is_rp_psw),

      .is_mov    (is_mov),
      .is_sphl   (is_sphl),
      .is_mvi    (is_mvi),
      .is_lxi    (is_lxi),
      .is_lda    (is_lda),
      .is_sta    (is_sta),
      .is_lhld   (is_lhld),
      .is_shld   (is_shld),
      .is_ldax   (is_ldax),
      .is_stax   (is_stax),
      .is_xchg   (is_xchg),
      .is_alu_reg(is_alu_reg),
      .is_alu_imm(is_alu_imm),
      .is_alu_alt(is_alu_alt),
      .is_inr    (is_inr),
      .is_dcr    (is_dcr),
      .is_inx    (is_inx),
      .is_dcx    (is_dcx),
      .is_dad    (is_dad),
      .is_jmp    (is_jmp),
      .is_call   (is_call),
      .is_ret    (is_ret),
      .is_rst    (is_rst),
      .is_pchl   (is_pchl),
      .is_push   (is_push),
      .is_pop    (is_pop),
      .is_xthl   (is_xthl),
      .is_in     (is_in),
      .is_out    (is_out),
      .is_ei     (is_ei),
      .is_di     (is_di),
      .is_hlt    (is_hlt),
      .is_nop    (is_nop),

      .use_branch_cond(use_branch_cond),
      .branch_cond    (branch_cond),

      .iint (int_sync),
      .ready(ready),
      .wwait(wwait),
      .inte (inte),

      .sync  (sync),
      .status(status),

      .data_in_enable (data_in_enable),
      .data_out_enable(data_out_enable),
      .write_data_out (write_data_out),

      .read_flags (read_flags),
      .write_flags(write_flags),
      .read_a     (read_a),
      .write_a    (write_a),
      .write_act  (write_act),
      .read_tmp   (read_tmp),
      .write_tmp  (write_tmp),
      .write_instr(write_instr),
      .read_alu   (read_alu),
      .read_regs  (read_regs),
      .write_regs (write_regs),

      .swap_hl_de       (swap_hl_de),
      .cpy_hl_to_sp     (cpy_hl_to_sp),
      .cpy_hl_to_pc     (cpy_hl_to_pc),
      .cpy_wz_to_hl     (cpy_wz_to_hl),
      .cpy_wz_next_to_pc(cpy_wz_next_to_pc),
      .write_wz_dup     (write_wz_dup),
      .write_wz_rst     (write_wz_rst),

      .reg_sel    (reg_sel),
      .alu_control(alu_control),
      .write_adr  (write_adr),
      .inc_rp     (inc_rp),
      .dec_rp     (dec_rp),

      .a_src    (a_src),
      .act_src  (act_src),
      .flags_src(flags_src),

      .dbin (dbin),
      .write(write)
  );

  register #(
      .WIDTH(2 * XLEN)
  ) adr_reg (
      .clk  (clk),
      .rst(rst),

      .wenable(write_adr),

      .in (regs_out_rp),
      .out(addr)
  );

  data_bus_buffer #(
      .XLEN(XLEN)
  ) data_bus_buffer (
      .clk  (clk),
      .rst(rst),

      .bus        (bus),
      .status     (status),
      .sync       (sync),
      .out_wenable(write_data_out),
      .out_enable (data_out_enable),
      .in_enable  (data_in_enable),

      .out(data)
  );

  assign write_n = ~write;
endmodule
