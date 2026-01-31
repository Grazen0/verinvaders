`default_nettype none `timescale 1ns / 1ps

`include "i8080.vh"

module control #(
    parameter XLEN = 8
) (
    input wire clk,
    input wire rst,

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
    if (rst) begin
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

