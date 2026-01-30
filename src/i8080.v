`default_nettype none `timescale 1ns / 1ps

`include "i8080.vh"

module data_bus_buffer #(
    parameter XLEN = 8
) (
    input wire clk,
    input wire rst_n,

    inout wire [XLEN-1:0] bus,
    input wire            sync,
    input wire [XLEN-1:0] status,
    input wire            out_wenable,
    input wire            out_enable,
    input wire            in_enable,

    inout tri [XLEN-1:0] out

);
  localparam ZZ = {XLEN{1'bz}};

  wire [XLEN-1:0] out_data;

  register #(
      .WIDTH(XLEN)
  ) out_data_reg (
      .clk  (clk),
      .rst_n(rst_n),

      .wenable(out_wenable),
      .in     (bus),
      .out    (out_data)
  );

  assign out = out_enable ? out_data : ZZ;
  assign out = sync ? status : ZZ;

  assign bus = in_enable ? out : ZZ;
endmodule


module control #(
    parameter XLEN = 8
) (
    input wire clk,
    input wire rst_n,

    input wire [2:0] sss,
    input wire [2:0] ddd,
    input wire [1:0] rp,
    input wire [2:0] alu_op,

    input wire is_sss_mem,
    input wire is_sss_a,
    input wire is_ddd_mem,
    input wire is_ddd_a,
    input wire is_alu_op_cmp,
    input wire is_rp_psw,

    input wire is_mov,
    input wire is_sphl,
    input wire is_mvi,
    input wire is_lxi,
    input wire is_lda,
    input wire is_sta,
    input wire is_lhld,
    input wire is_shld,
    input wire is_ldax,
    input wire is_stax,
    input wire is_xchg,
    input wire is_alu_reg,
    input wire is_alu_imm,
    input wire is_alu_alt,
    input wire is_inr,
    input wire is_dcr,
    input wire is_inx,
    input wire is_dcx,
    input wire is_dad,
    input wire is_jmp,
    input wire is_call,
    input wire is_ret,
    input wire is_rst,
    input wire is_pchl,
    input wire is_push,
    input wire is_pop,
    input wire is_xthl,
    input wire is_in,
    input wire is_out,
    input wire is_ei,
    input wire is_di,
    input wire is_hlt,
    input wire is_nop,

    input wire use_branch_cond,
    input wire branch_cond,

    input  wire iint,
    input  wire ready,
    output wire wwait,
    output wire sync,
    output reg  inte,

    output reg data_in_enable,
    output reg data_out_enable,
    output reg write_data_out,

    output reg a_src,
    output reg act_src,
    output reg flags_src,

    output wire write,
    output wire dbin,

    output reg read_flags,
    output reg read_a,
    output reg read_tmp,
    output reg read_alu,
    output reg read_regs,

    output reg write_flags,
    output reg write_a,
    output reg write_act,
    output reg write_tmp,
    output reg write_instr,
    output reg write_regs,

    output reg swap_hl_de,
    output reg cpy_hl_to_sp,
    output reg cpy_hl_to_pc,
    output reg cpy_wz_to_hl,
    output reg cpy_wz_next_to_pc,
    output reg write_wz_dup,
    output reg write_wz_rst,

    output reg [3:0] reg_sel,
    output reg [4:0] alu_control,
    output reg       write_adr,
    output reg       inc_rp,
    output reg       dec_rp,

    output reg [XLEN-1:0] status
);
  localparam T1 = 3'd0;
  localparam T2 = 3'd1;
  localparam T3 = 3'd2;
  localparam T4 = 3'd3;
  localparam T5 = 3'd4;
  localparam TR = 3'd5;
  localparam TW = 3'd6;
  localparam TWH = 3'd7;
  localparam TZ = 3'dz;

  localparam M1 = 3'd0;
  localparam M2 = 3'd1;
  localparam M3 = 3'd2;
  localparam M4 = 3'd3;
  localparam M5 = 3'd4;
  localparam MZ = 3'dz;

  wire inc_mcycle;

  reg [2:0] tstate, tstate_next;
  reg [2:0] mcycle, mcycle_next;

  reg mcycle_end, instr_end;
  reg read_sss, read_ddd, write_ddd;
  reg wz_as_pc_next;

  wire [3:0] sss_ext = {1'b0, sss};
  wire [3:0] ddd_ext = {1'b0, ddd};
  wire [2:0] rp_ext = {1'b0, rp};

  reg wz_as_pc;
  reg inte_next;
  reg iint_prev, iint_prev_next;
  reg int_ff, int_ff_next;
  reg hlt;

  reg [XLEN-1:0] status_next;

  always @(*) begin
    inte_next                 = inte;
    int_ff_next               = int_ff;
    iint_prev_next            = iint;

    status_next               = status;
    status_next[`STATUS_INTA] = 0;
    status_next[`STATUS_INP]  = 0;
    status_next[`STATUS_OUT]  = 0;

    inc_rp                    = 0;
    dec_rp                    = 0;

    data_in_enable            = 0;
    data_out_enable           = 0;
    write_data_out            = 0;

    a_src                     = `A_SRC_BUS;
    act_src                   = `ACT_SRC_A;
    flags_src                 = `FLAGS_SRC_ALU;

    reg_sel                   = 4'bxxxx;
    alu_control               = 5'bxxxxx;

    read_flags                = 0;
    write_flags               = 0;
    write_a                   = 0;
    read_a                    = 0;
    write_act                 = 0;
    read_tmp                  = 0;
    write_tmp                 = 0;
    write_instr               = 0;
    read_regs                 = 0;
    write_regs                = 0;
    read_alu                  = 0;

    swap_hl_de                = 0;
    cpy_hl_to_sp              = 0;
    cpy_hl_to_pc              = 0;
    cpy_wz_to_hl              = 0;
    cpy_wz_next_to_pc         = 0;
    write_wz_dup              = 0;
    write_wz_rst              = 0;

    write_adr                 = 0;

    mcycle_end                = 0;
    instr_end                 = 0;

    read_sss                  = 0;
    read_ddd                  = 0;
    write_ddd                 = 0;

    wz_as_pc_next             = 0;

    hlt                       = 0;

    if (inte && iint && ~iint_prev) begin
      int_ff_next = 1;
    end

    // verilog_format: off
    casez ({mcycle, tstate})
      {MZ, TR}: begin
        instr_end = 1;
      end

      {MZ, TWH}: begin
        hlt = 1;
      end

      {M1, T1}: begin
        if (!wz_as_pc) begin
          reg_sel = {`RP_SEL_PC, 1'bx};
        end else begin
          reg_sel           = {`RP_SEL_WZ, 1'bx};
          cpy_wz_next_to_pc = 1;
        end

        if (!status[`STATUS_INTA]) begin
          inc_rp = 1;
        end
      end
      {M1, T2}, {M1, TW}: begin
        data_in_enable = 1;
        write_instr    = 1;
      end
      {M1, T3}: begin
        if (is_mov && !is_sss_mem) begin
          read_sss = 1;
          write_tmp = 1;
        end

        if (is_sphl) begin
          cpy_hl_to_sp = 1;
        end

        if (is_pchl) begin
          cpy_hl_to_pc = 1;
        end

        if (is_xchg) begin
          swap_hl_de = 1;
        end

        if (is_alu_reg) begin
          if (!is_sss_mem) begin
            write_tmp = 1;
            read_sss  = 1;
          end

          write_act = 1;
        end

        if (is_alu_imm || is_alu_alt) begin
          write_act = 1;
        end

        if ((is_inr || is_dcr) && !is_ddd_mem) begin
          read_ddd  = 1;
          write_tmp = 1;
        end

        if (is_ei) begin
          inte_next = 1;
        end else if (is_di) begin
          inte_next = 0;
        end
      end
      {M1, T4}: begin
        if (is_mov) begin
          if (!is_sss_mem && !is_ddd_mem) begin
            read_tmp  = 1;
            write_ddd = 1;
          end else begin
            mcycle_end = 1;

            reg_sel   = {`RP_SEL_HL, 1'bx};
            write_adr = 1;
          end
        end

        if (is_mvi || is_lxi || is_lda || is_sta || is_lhld || is_shld || is_jmp
            || is_in || is_out || is_hlt) begin
          mcycle_end = 1;
          reg_sel = {`RP_SEL_PC, 1'bx};
          write_adr = 1;
        end

        if (is_hlt) begin
          status_next[`STATUS_HLTA] = 1;
        end

        if (is_ldax || is_stax) begin
          mcycle_end = 1;

          reg_sel = {rp_ext, 1'bx};
          write_adr = 1;
        end

        if (is_xchg || is_nop || is_ei || is_di) begin
          instr_end = 1;
        end

        if (is_alu_reg) begin
          mcycle_end = 1;

          if (is_sss_mem) begin
            reg_sel   = {`RP_SEL_HL, 1'bx};
            write_adr = 1;
          end else begin
            alu_control = {2'b00, alu_op};
            a_src       = `A_SRC_ALU;
            write_a     = ~is_alu_op_cmp;
            write_flags = 1;

            instr_end = 1;
          end
        end

        if (is_alu_imm) begin
          mcycle_end = 1;

          reg_sel   = {`RP_SEL_PC, 1'bx};
          write_adr = 1;
        end

        if (is_alu_alt) begin
            alu_control = {2'b01, alu_op};
            a_src       = `A_SRC_ALU;
            write_a     = 1;
            write_flags = 1;

            instr_end = 1;
        end

        if (is_inr || is_dcr) begin
          if (is_ddd_mem) begin
            reg_sel    = {`RP_SEL_HL, 1'bx};
            write_adr  = 1;
            mcycle_end = 1;
          end else begin
            alu_control = {4'b1000, is_dcr}; // TODO: give name to this
            read_alu = 1;
            write_flags = 1;
            write_ddd = 1;
          end
        end

        if (is_inx) begin
          reg_sel = {rp_ext, 1'bx};
          inc_rp  = 1;
        end

        if (is_dcx) begin
          reg_sel = {rp_ext, 1'bx};
          dec_rp  = 1;
        end

        if (is_dad) begin
          reg_sel = {rp_ext, `RP_LO};
          read_regs = 1;

          act_src = `ACT_SRC_BUS;
          write_act = 1;

          mcycle_end = 1;
        end

        if ((is_call && branch_cond) || is_rst || is_push) begin
          reg_sel = {`RP_SEL_SP, 1'bx};
          dec_rp = 1;
        end

        if ((is_ret && !use_branch_cond) || is_pop || is_xthl) begin
          mcycle_end = 1;
          reg_sel    = {`RP_SEL_SP, 1'bx};
          write_adr  = 1;
        end
      end
      {M1, T5}: begin
        mcycle_end = 1;
        instr_end  = 1;

        if (is_call) begin
          instr_end = 0;
          reg_sel   = {`RP_SEL_PC, 1'bx};
          write_adr = 1;
        end

        if (is_ret && branch_cond) begin // is_ret here implies use_branch_cond
          instr_end = 0;
          reg_sel    = {`RP_SEL_SP, 1'bx};
          write_adr  = 1;
        end

        if (is_rst || is_push) begin
          instr_end = 0;
          reg_sel   = {`RP_SEL_SP, 1'bx};
          dec_rp    = 1;
          write_adr = 1;
        end
      end

      {M2, T1}: begin
        if (is_mov) begin
          if (is_ddd_mem) begin
            write_data_out = 1;
            read_tmp = 1;
          end
        end

        if (is_mvi || is_lxi || is_lda || is_sta || is_lhld || is_shld || is_alu_imm
            || is_jmp || is_call || is_in || is_out) begin
          reg_sel = {`RP_SEL_PC, 1'bx};
          inc_rp = 1;
        end

        if (is_stax) begin
          read_a = 1;
          write_data_out = 1;
        end

        if (is_dad) begin
          reg_sel   = `REG_SEL_L;
          read_regs = 1;
          write_tmp = 1;
        end

        if (is_ret || is_pop || is_xthl) begin
          reg_sel = {`RP_SEL_SP, 1'bx};
          inc_rp  = 1;
        end

        if (is_rst) begin
          reg_sel        = `REG_SEL_PC_HI;
          read_regs      = 1;
          write_data_out = 1;
        end

        if (is_push) begin
          if (is_rp_psw) begin
            read_a = 1;
          end else begin
            reg_sel   = {rp_ext, `RP_HI};
            read_regs = 1;
          end

          write_data_out = 1;
        end
      end

      {M2, T2}, {M2, TW}: begin
        if (is_mov) begin
          if (is_sss_mem) begin
            data_in_enable = 1;
            write_ddd = 1;
          end else begin
            data_out_enable = 1;
          end
        end

        if (is_mvi) begin
          data_in_enable = 1;

          if (is_ddd_mem) begin
            write_tmp = 1;
          end else begin
            write_ddd = 1;
          end
        end

        if (is_lxi) begin
          data_in_enable = 1;
          write_regs     = 1;
          reg_sel        = {rp_ext, `RP_LO};
        end

        if (is_lda || is_sta || is_lhld || is_shld || is_jmp || is_call || is_ret || is_xthl) begin
          data_in_enable = 1;
          reg_sel        = `REG_SEL_Z;
          write_regs     = 1;
        end

        if (is_ldax) begin
          data_in_enable = 1;
          write_a        = 1;
        end

        if (is_stax) begin
          data_out_enable = 1;
        end

        if (is_alu_reg || is_alu_imm) begin // is_alu_reg implies is_sss_mem
          data_in_enable = 1;
          write_tmp      = 1;
        end

        if (is_inr || is_dcr) begin
          data_in_enable = 1;
          write_tmp      = 1;
        end

        if (is_dad) begin
          alu_control = 5'b11000;
          read_alu   = 1;

          reg_sel     = `REG_SEL_L;
          write_regs  = 1;
          write_flags = 1;
        end

        if (is_rst || is_push) begin
          data_out_enable = 1;
        end

        if (is_pop) begin
          data_in_enable = 1;

          if (is_rp_psw) begin
            flags_src = `FLAGS_SRC_BUS;
            write_flags = 1;
          end else begin
            reg_sel    = {rp_ext, `RP_LO};
            write_regs = 1;
          end
        end

        if (is_in || is_out) begin
          data_in_enable = 1;
          write_wz_dup   = 1;
        end

        if (is_hlt) begin
          hlt = 1;
        end
      end

      {M2, T3}: begin
        mcycle_end = 1; // No instruction has an M2 cycle over 3 T-states long

        if (is_mov || is_ldax || is_stax) begin
          instr_end = 1;
        end

        if (is_mvi) begin
          if (!is_ddd_mem) begin
            instr_end = 1;
          end else begin
            write_adr = 1;
            reg_sel = {`RP_SEL_HL, 1'bx};
          end
        end

        if (is_lxi || is_lda || is_sta || is_lhld || is_shld || is_jmp || is_call) begin
          write_adr = 1;
          reg_sel = {`RP_SEL_PC, 1'bx};
        end

        if (is_alu_reg || is_alu_imm) begin  // is_alu_reg implies is_sss_mem
          alu_control = {2'b00, alu_op};
          a_src       = `A_SRC_ALU;
          write_a     = ~is_alu_op_cmp;
          write_flags = 1;

          instr_end = 1;
        end

        if (is_inr || is_dcr) begin
          reg_sel = {`RP_SEL_HL, 1'bx};
          write_adr = 1;
        end

        if (is_dad) begin
          reg_sel = {rp_ext, `RP_HI};
          read_regs = 1;

          act_src = `ACT_SRC_BUS;
          write_act = 1;
        end

        if (is_ret || is_pop || is_xthl) begin
          reg_sel    = {`RP_SEL_SP, 1'bx};
          write_adr  = 1;
        end

        if (is_rst || is_push) begin
          reg_sel    = {`RP_SEL_SP, 1'bx};
          write_adr  = 1;
        end

        if (is_in || is_out) begin
          reg_sel    = {`RP_SEL_WZ, 1'bx};
          write_adr  = 1;
        end

        if (is_in) status_next[`STATUS_INP] = 1;
        if (is_out) status_next[`STATUS_OUT] = 1;
      end

      {M3, T1}: begin
        if (is_mvi) begin
          read_tmp       = 1;
          write_data_out = 1;
        end

        if (is_lxi || is_lda || is_sta || is_lhld || is_shld || is_jmp || is_call) begin
          reg_sel = {`RP_SEL_PC, 1'bx};
          inc_rp = 1;
        end

        if (is_inr || is_dcr) begin
          alu_control = {4'b1000, is_dcr};
          read_alu    = 1;
          write_flags = 1;
          write_data_out = 1;
        end

        if (is_dad) begin
          reg_sel   = `REG_SEL_H;
          read_regs = 1;
          write_tmp = 1;
        end

        if (is_ret || is_pop) begin
          reg_sel = {`RP_SEL_SP, 1'bx};
          inc_rp  = 1;
        end

        if (is_rst) begin
          reg_sel = `REG_SEL_PC_LO;
          read_regs      = 1;
          write_data_out = 1;
        end

        if (is_push) begin
          if (is_rp_psw) begin
            read_flags = 1;
          end else begin
            reg_sel   = {rp_ext, `RP_LO};
            read_regs = 1;
          end

          write_data_out = 1;
        end

        if (is_out) begin
          read_a         = 1;
          write_data_out = 1;
        end
      end
      {M3, T2}, {M3, TW}: begin
        if (is_mvi || is_inr || is_dcr) begin
          data_out_enable = 1;
        end

        if (is_lxi) begin
          data_in_enable = 1;
          write_regs     = 1;
          reg_sel        = {rp_ext, `RP_HI};
        end

        if (is_lda || is_sta || is_lhld || is_shld || is_jmp || is_call) begin
          data_in_enable = 1;
          reg_sel        = `REG_SEL_W;
          write_regs     = 1;
        end

        if (is_dad) begin
          alu_control = 5'b11001; // TODO: add name to this
          read_alu    = 1;

          reg_sel     = `REG_SEL_H;
          write_regs  = 1;
          write_flags = 1;
        end

        if (is_ret || is_xthl) begin
          data_in_enable = 1;
          reg_sel        = `REG_SEL_W;
          write_regs     = 1;
        end

        if (is_push) begin
          data_out_enable = 1;
        end

        if (is_rst) begin
          data_out_enable = 1;
          write_wz_rst    = 1;
        end

        if (is_pop) begin
          data_in_enable = 1;

          if (is_rp_psw) begin
            write_a = 1;
          end else begin
            reg_sel    = {rp_ext, `RP_HI};
            write_regs = 1;
          end
        end

        if (is_in) begin
          data_in_enable = 1;
          write_a        = 1;
        end

        if (is_out) begin
          data_out_enable = 1;
        end
      end
      {M3, T3}: begin
        mcycle_end = 1; // No instruction has an M3 cycle over 3 T-states long

        if (is_mvi || is_lxi || is_inr || is_dcr || is_dad || is_push || is_pop
            || is_in || is_out) begin
          instr_end = 1;
        end

        if (is_lda || is_sta || is_lhld || is_shld) begin
          reg_sel   = {`RP_SEL_WZ, 1'bx};
          write_adr = 1;
          inc_rp    = 1; // for lhld and shld
        end

        if (is_jmp) begin
          if (branch_cond) begin
            wz_as_pc_next = 1;
          end
          instr_end = 1;
        end

        if (is_call || is_xthl) begin
          if (branch_cond) begin
            reg_sel   = {`RP_SEL_SP, 1'bx};
            write_adr = 1;
            dec_rp    = 1;
          end else begin
            instr_end = 1;
          end
        end

        if (is_ret || is_rst) begin
          instr_end     = 1;
          wz_as_pc_next = 1;
        end
      end

      {M4, T1}: begin
        if (is_sta) begin
          read_a         = 1;
          write_data_out = 1;
        end

        if (is_shld) begin
          reg_sel        = `REG_SEL_L;
          read_regs      = 1;
          write_data_out = 1;
        end

        if (is_call) begin
          reg_sel        = `REG_SEL_PC_HI;
          read_regs      = 1;
          write_data_out = 1;
        end

        if (is_xthl) begin
          reg_sel        = `REG_SEL_H;
          read_regs      = 1;
          write_data_out = 1;
        end
      end

      {M4, T2}, {M4, TW}: begin
        if (is_lda) begin
          data_in_enable = 1;
          write_a        = 1;
        end

        if (is_sta || is_shld || is_call || is_xthl) begin
          data_out_enable = 1;
        end

        if (is_lhld) begin
          reg_sel = `REG_SEL_L;
          write_regs = 1;
          data_in_enable = 1;
        end
      end

      {M4, T3}: begin
        mcycle_end = 1; // No instruction has an M4 cycle over 3 T-states long

        if (is_lda || is_sta) begin
          instr_end = 1;
        end

        if (is_lhld || is_shld) begin
          reg_sel   = {`RP_SEL_WZ, 1'bx};
          write_adr = 1;
        end

        if (is_call || is_xthl) begin
          reg_sel   = {`RP_SEL_SP, 1'bx};
          write_adr = 1;
        end
      end

      {M5, T1}: begin
        if (is_shld) begin
          reg_sel = `REG_SEL_H;
          read_regs = 1;
          write_data_out = 1;
        end

        if (is_call) begin
          reg_sel = `REG_SEL_PC_LO;
          read_regs = 1;
          write_data_out = 1;
        end

        if (is_xthl) begin
          reg_sel = `REG_SEL_L;
          read_regs = 1;
          write_data_out = 1;
        end
      end

      {M5, T2}, {M5, TW}: begin
        if (is_lhld) begin
          reg_sel = `REG_SEL_H;
          write_regs = 1;
          data_in_enable = 1;
        end

        if (is_shld || is_call || is_xthl) begin
          data_out_enable = 1;
        end
      end

      {M5, T3}: begin
        if (is_lhld || is_shld) begin
          instr_end = 1;
        end

        if (is_call) begin
          instr_end = 1;
          wz_as_pc_next = 1;
        end
      end
      {M5, T4}: begin // implies is_xthl
          cpy_wz_to_hl = 1;
      end
      {M5, T5}: begin // implies is_xthl
        instr_end = 1;
      end

      default: begin
        $display("oopsie daisy (mcycle = %h, tstate = %h)", mcycle, tstate);
      end
    endcase
    // verilog_format: on

    if (read_sss) begin
      if (is_sss_a) begin
        read_a = 1;
      end else begin
        reg_sel   = sss_ext;
        read_regs = 1;
      end
    end

    if (read_ddd) begin
      if (is_ddd_a) begin
        read_a = 1;
      end else begin
        reg_sel   = ddd_ext;
        read_regs = 1;
      end
    end

    if (write_ddd) begin
      if (is_ddd_a) begin
        write_a = 1;
      end else begin
        reg_sel    = ddd_ext;
        write_regs = 1;
      end
    end

    if (hlt) begin
      mcycle_next = M1;
      tstate_next = TWH;

      if (int_ff) begin
        int_ff_next = 0;
        tstate_next = T1;

        reg_sel = {`RP_SEL_PC, 1'bx};
        write_adr = 1;

        status_next[`STATUS_INTA] = 1;
      end
    end else if ((tstate == T2 || tstate == TW) && !ready) begin
      mcycle_next = mcycle;
      tstate_next = TW;
    end else if (instr_end) begin
      mcycle_next = M1;
      tstate_next = T1;

      reg_sel = {wz_as_pc_next ? `RP_SEL_WZ : `RP_SEL_PC, 1'bx};
      write_adr = 1;

      if (int_ff) begin
        int_ff_next = 0;
        status_next[`STATUS_INTA] = 1;
      end
    end else if (mcycle_end) begin
      mcycle_next = mcycle + 1;
      tstate_next = T1;
    end else begin
      mcycle_next = mcycle;
      tstate_next = tstate + 1;
    end

    status_next[`STATUS_M1] = mcycle_next == M1;
  end

  always @(posedge clk) begin
    if (!rst_n) begin
      inte      <= 0;
      int_ff    <= 0;
      iint_prev <= 0;
      status    <= 0;
      tstate    <= TR;
      wz_as_pc  <= 0;
    end else begin
      inte      <= inte_next;
      int_ff    <= int_ff_next;
      iint_prev <= iint_prev_next;
      status    <= status_next;
      tstate    <= tstate_next;
      mcycle    <= mcycle_next;
      wz_as_pc  <= wz_as_pc_next;
    end
  end

  assign dbin  = data_in_enable;
  assign write = data_out_enable;
  assign sync  = tstate == T1;
  assign wwait = tstate == TW | tstate == TWH;
endmodule

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

module decimal_adjust #(
    parameter XLEN = 8
) (
    input wire [XLEN-1:0] in,
    input wire [XLEN-1:0] flags_in,

    output wire [XLEN-1:0] out,
    output reg  [XLEN-1:0] flags_out
);
  reg [XLEN/2-1:0] d0, d1;

  always @(*) begin
    {d1, d0}  = in;
    flags_out = flags_in;

    if (d0 > 9 || flags_out[`FA]) begin
      {d1, d0} = {d1, d0} + 6;
      flags_out[`FA] = 1;
    end

    if (d1 > 9 || flags_in[`FC]) begin
      d1             = d1 + 6;
      flags_out[`FC] = 1;
    end
  end

  assign out = {d1, d0};
endmodule

module alu #(
    parameter XLEN = 8
) (
    input wire [XLEN-1:0] op_a,
    input wire [XLEN-1:0] op_b,
    input wire [4:0] control,
    input wire [XLEN-1:0] flags_in,

    output reg [XLEN-1:0] out,
    output reg [XLEN-1:0] flags_out
);
  wire            carry_in = ~control[2] & control[0] & flags_in[`FC];

  wire [  XLEN:0] op_a_ext = {1'b0, op_a};
  wire [  XLEN:0] op_b_ext = {1'b0, op_b};
  wire [  XLEN:0] carry_ext = {{(XLEN) {1'b0}}, carry_in};

  wire [  XLEN:0] op_b_ext_signed = control[1] ? -op_b_ext : op_b_ext;
  wire [  XLEN:0] carry_ext_signed = control[1] ? -carry_ext : carry_ext;

  wire [XLEN/2:0] op_a_d0 = {1'b0, op_a[XLEN/2-1:0]};
  wire [XLEN/2:0] op_b_d0 = {1'b0, op_b[XLEN/2-1:0]};
  wire [XLEN/2:0] op_b_d0_signed = control[1] ? -op_b_d0 : op_b_d0;

  // TODO: remove unnecessary extra sum just for the aux carry
  wire [XLEN/2:0] aux_sum = op_a_d0 + op_b_d0_signed + carry_ext_signed[XLEN/2:0];
  wire [  XLEN:0] sum = op_a_ext + op_b_ext_signed + carry_ext_signed;

  reg             set_pzs_flags;

  wire [XLEN-1:0] daa_flags_out;
  wire [XLEN-1:0] daa_out;

  decimal_adjust #(
      .XLEN(XLEN)
  ) decimal_adjust (
      .in      (op_a),
      .flags_in(flags_in),

      .out      (daa_out),
      .flags_out(daa_flags_out)
  );

  always @(*) begin
    flags_out = flags_in;
    set_pzs_flags = 0;

    casez (control)
      5'b00_0zz, 5'b00_111: begin  // add/adc/sbb/sbc
        {flags_out[`FC], out} = sum;
        flags_out[`FA]        = aux_sum[XLEN/2];
        set_pzs_flags         = 1;
      end
      5'b00_100: begin  // ana
        out            = op_a & op_b;
        flags_out[`FC] = 0;
        flags_out[`FA] = 0;
        set_pzs_flags  = 1;
      end
      5'b00_101: begin  // xra
        out            = op_a ^ op_b;
        flags_out[`FC] = 0;
        flags_out[`FA] = 0;
        set_pzs_flags  = 1;
      end
      5'b00_110: begin  // ora
        out            = op_a | op_b;
        flags_out[`FC] = 0;
        flags_out[`FA] = 0;
        set_pzs_flags  = 1;
      end
      5'b01_000: begin  // rlc
        out            = {op_a[XLEN-2:0], op_a[XLEN-1]};
        flags_out[`FC] = op_a[XLEN-1];
      end
      5'b01_001: begin  // rrc
        out            = {op_a[0], op_a[XLEN-1:1]};
        flags_out[`FC] = op_a[0];
      end
      5'b01_010: {flags_out[`FC], out} = {op_a, flags_out[`FC]};  // ral
      5'b01_011: {out, flags_out[`FC]} = {flags_out[`FC], op_a};  // rar
      5'b01_100: begin  // daa
        out    = daa_out;
        flags_out = daa_flags_out;
      end
      5'b01_101: out = ~op_a;  // cma
      5'b01_110: begin  // stc
        out            = op_a;
        flags_out[`FC] = 1;
      end
      5'b01_111: begin  // cmc
        out            = op_a;
        flags_out[`FC] = ~flags_out[`FC];
      end

      5'b10_zz0: begin  // inr
        out           = op_b + 1;
        set_pzs_flags = 1;
      end
      5'b10_zz1: begin  // dcr
        out           = op_b - 1;
        set_pzs_flags = 1;
      end

      5'b11_zzz: {flags_out[`FC], out} = sum;  // add but only set carry, used for dad

      default: out = {XLEN{1'bx}};
    endcase

    if (set_pzs_flags) begin
      flags_out[`FP] = ~(^out);
      flags_out[`FZ] = out == 0;
      flags_out[`FS] = out[XLEN-1];
    end
  end
endmodule

module register_array #(
    parameter XLEN = 8
) (
    input wire clk,
    input wire rst_n,

    input wire [     3:0] reg_sel,
    input wire [XLEN-1:0] wdata,
    input wire            wenable,

    output reg  [2*XLEN-1:0] rpdata,
    output wire [  XLEN-1:0] rdata,

    // why
    input wire swap_hl_de,
    input wire cpy_hl_to_sp,
    input wire cpy_hl_to_pc,
    input wire cpy_wz_to_hl,
    input wire inc,
    input wire dec,
    input wire cpy_wz_next_to_pc,
    input wire [XLEN-1:0] instr,
    input wire write_wz_dup,
    input wire write_wz_rst
);
  reg [XLEN-1:0] w, z, b, c, d, e, h, l;
  reg [XLEN-1:0] w_next, z_next, b_next, c_next, d_next, e_next, h_next, l_next;

  reg [2*XLEN-1:0] pc, sp, pc_next, sp_next;

  wire [2:0] rp_sel;
  wire       nib_sel;

  assign {rp_sel, nib_sel} = reg_sel;

  wire [2*XLEN-1:0] inc_dec_result = inc ? rpdata + 1 : rpdata - 1;

  always @(*) begin
    case (rp_sel)
      `RP_SEL_BC: rpdata = {b, c};
      `RP_SEL_DE: rpdata = {d, e};
      `RP_SEL_HL: rpdata = {h, l};
      `RP_SEL_SP: rpdata = sp;
      `RP_SEL_WZ: rpdata = {w, z};
      `RP_SEL_PC: rpdata = pc;
      default:    rpdata = {(2 * XLEN) {1'bx}};
    endcase

    pc_next = pc;
    sp_next = sp;
    w_next  = w;
    z_next  = z;
    b_next  = b;
    c_next  = c;
    d_next  = d;
    e_next  = e;
    h_next  = h;
    l_next  = l;

    if (wenable) begin
      case (reg_sel)
        `REG_SEL_B: b_next = wdata;
        `REG_SEL_C: c_next = wdata;
        `REG_SEL_D: d_next = wdata;
        `REG_SEL_E: e_next = wdata;
        `REG_SEL_H: h_next = wdata;
        `REG_SEL_L: l_next = wdata;
        `REG_SEL_W: w_next = wdata;
        `REG_SEL_Z: z_next = wdata;
        `REG_SEL_SP_HI: sp_next[15:8] = wdata;
        `REG_SEL_SP_LO: sp_next[7:0] = wdata;
        `REG_SEL_PC_HI: pc_next[15:8] = wdata;
        `REG_SEL_PC_LO: pc_next[7:0] = wdata;
        default: ;
      endcase
    end else if (inc || dec) begin
      case (rp_sel)
        `RP_SEL_WZ: {w_next, z_next} = inc_dec_result;
        `RP_SEL_BC: {b_next, c_next} = inc_dec_result;
        `RP_SEL_DE: {d_next, e_next} = inc_dec_result;
        `RP_SEL_HL: {h_next, l_next} = inc_dec_result;
        `RP_SEL_SP: sp_next = inc_dec_result;
        `RP_SEL_PC: pc_next = inc_dec_result;
        default:    ;
      endcase
    end else begin
      if (swap_hl_de) begin
        {h_next, l_next} = {d, e};
        {d_next, e_next} = {h, l};
      end

      if (cpy_hl_to_sp) begin
        sp_next = {h, l};
      end

      if (write_wz_rst) begin
        w_next = 8'h00;
        z_next = instr & 8'b00_111_000;
      end else if (write_wz_dup) begin
        w_next = wdata;
        z_next = wdata;
      end

      if (cpy_wz_to_hl) begin
        {h_next, l_next} = {w, z};
      end
    end

    if (cpy_wz_next_to_pc) begin
      pc_next = {w_next, z_next};
    end else if (cpy_hl_to_pc) begin
      pc_next = {h, l};
    end
  end

  always @(posedge clk) begin
    if (!rst_n) begin
      pc <= 0;
    end else begin
      pc <= pc_next;
      sp <= sp_next;
      w  <= w_next;
      z  <= z_next;
      b  <= b_next;
      c  <= c_next;
      d  <= d_next;
      e  <= e_next;
      h  <= h_next;
      l  <= l_next;
    end
  end

  assign rdata = nib_sel == `RP_LO ? rpdata[7:0] : rpdata[15:8];
endmodule

module i8080 #(
    parameter XLEN = 8
) (
    input wire clk,
    input wire rst_n,

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
      .rst_n(rst_n),

      .wenable(write_flags),

      .in (flags_src == `FLAGS_SRC_ALU ? alu_flags_out : bus),
      .out(flags)
  );

  register #(
      .WIDTH(XLEN)
  ) a_reg (
      .clk  (clk),
      .rst_n(rst_n),

      .wenable(write_a),

      .in (a_src == `A_SRC_BUS ? bus : alu_out),
      .out(a)
  );

  register #(
      .WIDTH(XLEN)
  ) act_reg (
      .clk  (clk),
      .rst_n(rst_n),

      .wenable(write_act),
      .in     (act_src == `ACT_SRC_A ? a : bus),
      .out    (act)
  );

  register #(
      .WIDTH(XLEN)
  ) tmp_reg (
      .clk  (clk),
      .rst_n(rst_n),

      .wenable(write_tmp),
      .in     (bus),
      .out    (tmp)
  );

  register #(
      .WIDTH(XLEN)
  ) instr_reg (
      .clk  (clk),
      .rst_n(rst_n),

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
      .rst_n(rst_n),

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
      .rst_n(rst_n),

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
      .rst_n(rst_n),

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
      .rst_n(rst_n),

      .wenable(write_adr),

      .in (regs_out_rp),
      .out(addr)
  );

  data_bus_buffer #(
      .XLEN(XLEN)
  ) data_bus_buffer (
      .clk  (clk),
      .rst_n(rst_n),

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
