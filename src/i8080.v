`default_nettype none `timescale 1ns / 1ps

`define FC 0
`define FP 2
`define FA 4 // TODO: implement this flag
`define FZ 6
`define FS 7

`define FLAGS_SRC_ALU 1'b0
`define FLAGS_SRC_BUS 1'b1

`define A_SRC_ALU 1'b0
`define A_SRC_BUS 1'b1

`define ACT_SRC_A 1'b0
`define ACT_SRC_BUS 1'b1

`define RP_SEL_BC 3'b000
`define RP_SEL_DE 3'b001
`define RP_SEL_HL 3'b010
`define RP_SEL_SP 3'b011
`define RP_SEL_WZ 3'b100
`define RP_SEL_PC 3'b101

`define RP_LO 1'b1
`define RP_HI 1'b0

`define REG_SEL_B {`RP_SEL_BC, `RP_HI}
`define REG_SEL_C {`RP_SEL_BC, `RP_LO}
`define REG_SEL_D {`RP_SEL_DE, `RP_HI}
`define REG_SEL_E {`RP_SEL_DE, `RP_LO}
`define REG_SEL_H {`RP_SEL_HL, `RP_HI}
`define REG_SEL_L {`RP_SEL_HL, `RP_LO}
`define REG_SEL_W {`RP_SEL_WZ, `RP_HI}
`define REG_SEL_Z {`RP_SEL_WZ, `RP_LO}

module data_bus_buffer #(
    parameter XLEN = 8
) (
    input wire clk,
    input wire rst_n,

    inout wire [XLEN-1:0] bus,
    input wire            out_wenable,
    input wire            out_enable,
    input wire            in_enable,

    inout tri [XLEN-1:0] out

);
  wire [XLEN-1:0] data;

  register #(
      .WIDTH(XLEN)
  ) out_reg (
      .clk  (clk),
      .rst_n(rst_n),

      .wenable(out_wenable),
      .oenable(out_enable),

      .in     (bus),
      .out_tri(out)
  );

  assign bus = in_enable ? out : {XLEN{1'bz}};
endmodule

module control (
    input wire clk,
    input wire rst_n,

    input wire [2:0] sss,
    input wire [2:0] ddd,
    input wire [1:0] rp,
    input wire [2:0] cc,
    input wire [2:0] alu_op,

    input wire is_sss_mem,
    input wire is_sss_a,
    input wire is_ddd_mem,
    input wire is_ddd_a,
    input wire is_alu_op_cmp,

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
    input wire is_nop,

    input  wire ready,
    output wire sync,

    output reg data_in_enable,
    output reg data_out_enable,
    output reg write_data_out,

    output reg a_src,
    output reg act_src,
    output reg flags_src,

    output wire write,
    output wire dbin,

    output reg read_flags,
    output reg write_flags,
    output reg write_a,
    output reg read_a,
    output reg write_act,
    output reg read_tmp,
    output reg write_tmp,
    output reg write_instr,
    output reg read_alu,
    output reg read_regs,
    output reg write_regs,

    output reg swap_hl_de,
    output reg cpy_hl_to_sp,

    output reg [3:0] reg_sel,
    output reg [4:0] alu_control,
    output reg       write_adr,
    output reg       inc_rp,
    output reg       dec_rp
);
  localparam T1 = 3'd0;
  localparam T2 = 3'd1;
  localparam T3 = 3'd2;
  localparam T4 = 3'd3;
  localparam T5 = 3'd4;
  localparam TR = 3'd5;
  localparam TZ = 3'dz;

  localparam M1 = 3'd0;
  localparam M2 = 3'd1;
  localparam M3 = 3'd2;
  localparam M4 = 3'd3;
  localparam M5 = 3'd4;
  localparam MZ = 3'dz;

  localparam STATUS_INSTR_FETCH = 8'b1010_0010;
  localparam STATUS_MEM_READ = 8'b1000_0010;
  localparam STATUS_MEM_WRITE = 8'b0000_0000;
  localparam STATUS_STACK_READ = 8'b1000_0110;
  localparam STATUS_STACK_WRITE = 8'b0000_0100;
  localparam STATUS_IN_READ = 8'b0100_0010;
  localparam STATUS_OUT_WRITE = 8'b0001_0000;
  localparam STATUS_INT_ACK = 8'b0010_0011;
  localparam STATUS_HALT_ACK = 8'b1000_1010;
  localparam STATUS_INT_ACK_W_HALT = 8'b0010_1011;

  wire inc_mcycle;

  reg [2:0] tstate, tstate_next;
  reg [2:0] mcycle, mcycle_next;

  reg mcycle_end, instr_end;
  reg read_sss, read_ddd, write_ddd;

  wire [3:0] sss_ext = {1'b0, sss};
  wire [3:0] ddd_ext = {1'b0, ddd};
  wire [2:0] rp_ext = {1'b0, rp};

  always @(*) begin
    inc_rp          = 0;
    dec_rp          = 0;

    data_in_enable  = 0;
    data_out_enable = 0;
    write_data_out  = 0;

    a_src           = `A_SRC_BUS;
    act_src         = `ACT_SRC_A;
    flags_src       = `FLAGS_SRC_ALU;

    reg_sel         = 4'bxxxx;
    alu_control     = 5'bxxxxx;

    read_flags      = 0;
    write_flags     = 0;
    write_a         = 0;
    read_a          = 0;
    write_act       = 0;
    read_tmp        = 0;
    write_tmp       = 0;
    write_instr     = 0;
    read_regs       = 0;
    write_regs      = 0;
    read_alu        = 0;

    swap_hl_de      = 0;
    cpy_hl_to_sp    = 0;

    write_adr       = 0;

    mcycle_end      = 0;
    instr_end       = 0;

    read_sss        = 0;
    read_ddd        = 0;
    write_ddd       = 0;

    // verilog_format: off
    casez ({mcycle, tstate})
      {MZ, TR}: begin
        instr_end = 1;
      end

      {M1, T1}: begin
        reg_sel = {`RP_SEL_PC, 1'bx};
        inc_rp = 1;
      end
      {M1, T2}: begin
        data_in_enable = 1;
        write_tmp      = 1;
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

        if (is_mvi || is_lxi || is_lda || is_sta || is_lhld || is_shld) begin
          mcycle_end = 1;

          reg_sel = {`RP_SEL_PC, 1'bx};
          write_adr = 1;
        end

        if (is_ldax || is_stax) begin
          mcycle_end = 1;

          reg_sel = {rp_ext, 1'bx};
          write_adr = 1;
        end

        if (is_xchg || is_nop) begin
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
      end
      {M1, T5}: begin
        mcycle_end = 1;

        if (is_mov || is_sphl || is_inr || is_dcr || is_inx || is_dcx) begin
          instr_end = 1;
        end
      end

      {M2, T1}: begin
        if (is_mov) begin
          if (is_ddd_mem) begin
            write_data_out = 1;
            read_tmp = 1;
          end
        end

        if (is_mvi || is_lxi || is_lda || is_sta || is_lhld || is_shld || is_alu_imm) begin
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
      end

      {M2, T2}: begin
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

        if (is_lda || is_sta || is_lhld || is_shld) begin
          data_in_enable = 1;
          write_regs     = 1;
          reg_sel        = `REG_SEL_Z;
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

        if (is_lxi || is_lda || is_sta || is_lhld || is_shld) begin
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
      end

      {M3, T1}: begin
        if (is_mvi) begin
          read_tmp       = 1;
          write_data_out = 1;
        end

        if (is_lxi || is_lda || is_sta || is_lhld || is_shld) begin
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
      end
      {M3, T2}: begin
        if (is_mvi || is_inr || is_dcr) begin
          data_out_enable = 1;
        end

        if (is_lxi) begin
          data_in_enable = 1;
          write_regs     = 1;
          reg_sel        = {rp_ext, `RP_HI};
        end

        if (is_lda || is_sta || is_lhld || is_shld) begin
          data_in_enable = 1;
          write_regs     = 1;
          reg_sel        = `REG_SEL_W;
        end

        if (is_dad) begin
          alu_control = 5'b11001; // TODO: add name to this
          read_alu   = 1;

          reg_sel     = `REG_SEL_H;
          write_regs  = 1;
          write_flags = 1;
        end
      end
      {M3, T3}: begin
        mcycle_end = 1; // No instruction has an M3 cycle over 3 T-states long

        if (is_mvi || is_lxi || is_inr || is_dcr) begin
          instr_end = 1;
        end

        if (is_lda || is_sta || is_lhld || is_shld) begin
          reg_sel   = {`RP_SEL_WZ, 1'bx};
          write_adr = 1;
          inc_rp    = 1; // for lhld and shld
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
      end

      {M4, T2}: begin
        if (is_lda) begin
          data_in_enable = 1;
          write_a        = 1;
        end

        if (is_sta) begin
          data_out_enable = 1;
        end

        if (is_lhld) begin
          reg_sel = `REG_SEL_L;
          write_regs = 1;
          data_in_enable = 1;
        end

        if (is_shld) begin
          data_out_enable = 1;
        end
      end

      {M4, T3}: begin
        mcycle_end = 1; // No instruction has an M4 cycle over 3 T-states long

        if (is_lda || is_sta) begin
          instr_end = 1;
        end

        if (is_lhld || is_shld) begin
          reg_sel = {`RP_SEL_WZ, 1'bx};
          write_adr = 1;
        end
      end

      {M5, T1}: begin
        if (is_shld) begin
          reg_sel = `REG_SEL_H;
          read_regs = 1;
          write_data_out = 1;
        end
      end

      {M5, T2}: begin
        if (is_lhld) begin
          reg_sel = `REG_SEL_H;
          write_regs = 1;
          data_in_enable = 1;
        end

        if (is_shld) begin
          data_out_enable = 1;
        end
      end

      {M5, T3}: begin
        if (is_lhld || is_shld) begin
          instr_end = 1;
        end
      end

      default: begin
        // $display("oopsie daisy (mcycle = %h, tstate = %h)", mcycle, tstate);
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

    if (tstate == T2 && !ready) begin
      mcycle_next = mcycle;
      tstate_next = tstate;
    end else if (instr_end) begin
      mcycle_next = M1;
      tstate_next = T1;

      reg_sel = {`RP_SEL_PC, 1'bx};
      write_adr = 1;
    end else if (mcycle_end) begin
      mcycle_next = mcycle + 1;
      tstate_next = T1;
    end else begin
      mcycle_next = mcycle;
      tstate_next = tstate + 1;
    end
  end

  always @(posedge clk) begin
    if (!rst_n) begin
      tstate <= TR;
    end else begin
      tstate <= tstate_next;
      mcycle <= mcycle_next;
    end
  end

  assign dbin  = data_in_enable;
  assign write = data_out_enable;
  assign sync  = tstate == T1;
endmodule

module instr_decoder #(
    parameter XLEN = 8
) (
    input wire [XLEN-1:0] instr,

    output wire [2:0] sss,
    output wire [2:0] ddd,
    output wire [1:0] rp,
    output wire [2:0] cc,
    output wire [2:0] alu_op,

    output wire is_sss_mem,
    output wire is_sss_a,
    output wire is_ddd_mem,
    output wire is_ddd_a,
    output wire is_alu_op_cmp,

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
    output reg is_nop
);
  localparam REG_MEM = 3'b110;
  localparam REG_A = 3'b111;

  always @(*) begin
    is_mov     = 0;
    is_sphl    = 0;
    is_mvi     = 0;
    is_lxi     = 0;
    is_lda     = 0;
    is_sta     = 0;
    is_lhld    = 0;
    is_shld    = 0;
    is_ldax    = 0;
    is_stax    = 0;
    is_xchg    = 0;
    is_alu_reg = 0;
    is_alu_imm = 0;
    is_alu_alt = 0;
    is_inr     = 0;
    is_dcr     = 0;
    is_inx     = 0;
    is_dcx     = 0;
    is_dad     = 0;
    is_nop     = 0;

    casez (instr)
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
      default:       is_nop = 1;
    endcase
  end

  assign sss = instr[2:0];
  assign ddd = instr[5:3];
  assign rp = instr[5:4];
  assign cc = instr[5:3];
  assign alu_op = instr[5:3];

  assign is_sss_mem = sss == REG_MEM;
  assign is_sss_a = sss == REG_A;
  assign is_ddd_mem = ddd == REG_MEM;
  assign is_ddd_a = ddd == REG_A;
  assign is_alu_op_cmp = alu_op == 3'b111;
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
    input wire oenable,

    output wire [XLEN-1:0] out,
    output tri  [XLEN-1:0] out_tri,

    input  wire [XLEN-1:0] flags_in,
    output reg  [XLEN-1:0] flags_out
);
  reg  [XLEN-1:0] result;

  wire            carry_in = ~control[2] & control[0] & flags_in[`FC];

  wire [  XLEN:0] op_a_ext = {1'b0, op_a};
  wire [  XLEN:0] op_b_ext = {1'b0, op_b};
  wire [  XLEN:0] carry_ext = {{(XLEN) {1'b0}}, carry_in};

  wire [  XLEN:0] op_b_ext_signed = control[1] ? -op_b_ext : op_b_ext;
  wire [  XLEN:0] carry_ext_signed = control[1] ? -carry_ext : carry_ext;

  wire [  XLEN:0] sum = op_a_ext + op_b_ext_signed + carry_ext_signed;

  reg             set_pzs_flags;

  wire [XLEN-1:0] daa_flags_out;
  wire [XLEN-1:0] daa_out;

  decimal_adjust #(
      .XLEN(XLEN)
  ) decimal_adjust (
      .in(op_a),
      .flags_in(flags_in),

      .out(daa_out),
      .flags_out(daa_flags_out)
  );

  always @(*) begin
    flags_out = flags_in;
    set_pzs_flags = 0;

    // TODO: use A flag
    casez (control)
      5'b00_0zz, 5'b00_111: begin
        {flags_out[`FC], result} = sum;
        set_pzs_flags            = 1;
      end
      5'b00_100: begin
        result        = op_a & op_b;
        set_pzs_flags = 1;
      end
      5'b00_101: begin
        result        = op_a ^ op_b;
        set_pzs_flags = 1;
      end
      5'b00_110: begin
        result        = op_a | op_b;
        set_pzs_flags = 1;
      end
      5'b01_000: begin  // rlc
        result = {op_a[XLEN-2:0], op_a[XLEN-1]};
        flags_out[`FC] = op_a[XLEN-1];
      end
      5'b01_001: begin  // rrc
        result = {op_a[0], op_a[XLEN-1:1]};
        flags_out[`FC] = op_a[0];
      end
      5'b01_010: {flags_out[`FC], result} = {op_a, flags_out[`FC]};  // ral
      5'b01_011: {result, flags_out[`FC]} = {flags_out[`FC], op_a};  // rar
      5'b01_100: begin  // daa
        result = daa_out;
        flags_out = daa_flags_out;
      end
      5'b01_101: result = ~op_a;  // cma
      5'b01_110: begin  // stc
        result         = op_a;
        flags_out[`FC] = 1;
      end
      5'b01_111: begin  // cmc
        result         = op_a;
        flags_out[`FC] = ~flags_out[`FC];
      end

      5'b10_zz0: begin
        result        = op_b + 1;
        set_pzs_flags = 1;
      end
      5'b10_zz1: begin
        result        = op_b - 1;
        set_pzs_flags = 1;
      end

      5'b11_zzz: {flags_out[`FC], result} = sum;

      default: result = {XLEN{1'bx}};
    endcase

    if (set_pzs_flags) begin
      flags_out[`FP] = ~(^result);
      flags_out[`FZ] = result == 0;
      flags_out[`FS] = result[XLEN-1];
    end
  end

  assign out = result;
  assign out_tri = oenable ? out : {XLEN{1'bz}};
endmodule

module register_array #(
    parameter XLEN = 8
) (
    input wire clk,
    input wire rst_n,

    input wire [     3:0] reg_sel,
    input wire [XLEN-1:0] wdata,
    input wire            wenable,
    input wire            oenable,

    output reg [2*XLEN-1:0] rpdata,
    output tri [  XLEN-1:0] rdata,

    // why
    input wire swap_hl_de,
    input wire cpy_hl_to_sp,
    input wire inc,
    input wire dec,

    output reg [2*XLEN-1:0] pc
);
  reg [XLEN-1:0] w, z, b, c, d, e, h, l;
  reg  [2*XLEN-1:0] sp;

  wire [2*XLEN-1:0] pc_plus_1 = pc + 1;

  wire [       2:0] rp_sel;
  wire              nib_sel;

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
  end

  always @(posedge clk) begin
    if (!rst_n) begin
      pc <= 0;

      b  <= 1;
      c  <= 2;
      d  <= 3;
      e  <= 4;
      h  <= 5;
      l  <= 6;
    end else begin
      if (wenable) begin
        case (reg_sel)
          `REG_SEL_B: b <= wdata;
          `REG_SEL_C: c <= wdata;
          `REG_SEL_D: d <= wdata;
          `REG_SEL_E: e <= wdata;
          `REG_SEL_H: h <= wdata;
          `REG_SEL_L: l <= wdata;
          `REG_SEL_W: w <= wdata;
          `REG_SEL_Z: z <= wdata;
          default: ;
        endcase
      end else if (swap_hl_de) begin
        {h, l} <= {d, e};
        {d, e} <= {h, l};
      end else if (cpy_hl_to_sp) begin
        sp <= {h, l};
      end else if (inc || dec) begin
        case (rp_sel)
          `RP_SEL_BC: {b, c} = inc_dec_result;
          `RP_SEL_DE: {d, e} = inc_dec_result;
          `RP_SEL_HL: {h, l} = inc_dec_result;
          `RP_SEL_SP: sp = inc_dec_result;
          `RP_SEL_WZ: {w, z} = inc_dec_result;
          `RP_SEL_PC: pc = inc_dec_result;
          default:    ;
        endcase
      end
    end
  end

  wire [XLEN-1:0] out = nib_sel == `RP_LO ? rpdata[7:0] : rpdata[15:8];

  assign rdata = oenable ? out : {XLEN{1'bz}};
endmodule

module i8080 (
    input wire clk,
    input wire rst_n,

    inout  tri  [  XLEN-1:0] data,
    output wire [2*XLEN-1:0] addr,

    input  wire hold,
    output wire hlda,

    input  wire ready,
    output wire wwait,

    input  wire iint,
    output wire inte,

    output wire sync,

    output wire dbin,
    output wire write_n
);
  localparam XLEN = 8;

  tri [XLEN-1:0] bus;

  tri [XLEN-1:0] flags_tri;
  tri [XLEN-1:0] a_tri;
  tri [XLEN-1:0] act;
  tri [XLEN-1:0] tmp_tri;
  tri [XLEN-1:0] instr;
  tri [XLEN-1:0] alu_flags_out;
  tri [XLEN-1:0] alu_out_tri;
  tri [XLEN-1:0] regs_out;

  assign bus = a_tri;
  assign bus = tmp_tri;
  assign bus = flags_tri;
  assign bus = alu_out_tri;
  assign bus = regs_out;

  wire read_flags, write_flags;
  wire read_a, write_a;
  wire write_act;
  wire read_tmp, write_tmp;
  wire write_instr;
  wire read_alu;
  wire read_regs, write_regs;

  wire [XLEN-1:0] flags;

  register #(
      .WIDTH(XLEN),
      .RESET_VALUE(8'b0000_0010)
  ) flags_reg (
      .clk  (clk),
      .rst_n(rst_n),

      .wenable(write_flags),
      .oenable(read_flags),

      .in     (flags_src == `FLAGS_SRC_ALU ? alu_flags_out : bus),
      .out_tri(flags_tri),
      .out    (flags)
  );

  wire [XLEN-1:0] a;

  register #(
      .WIDTH(XLEN)
  ) a_reg (
      .clk  (clk),
      .rst_n(rst_n),

      .wenable(write_a),
      .oenable(read_a),

      .in(a_src == `A_SRC_BUS ? bus : alu_out),
      .out_tri(a_tri),
      .out(a)
  );

  register #(
      .WIDTH(XLEN)
  ) act_reg (
      .clk  (clk),
      .rst_n(rst_n),

      .wenable(write_act),
      .oenable(1'b1),

      .in (act_src == `ACT_SRC_A ? a : bus),
      .out(act)
  );

  wire [XLEN-1:0] tmp;

  register #(
      .WIDTH(XLEN)
  ) tmp_reg (
      .clk  (clk),
      .rst_n(rst_n),

      .wenable(write_tmp),
      .oenable(read_tmp),

      .in(bus),
      .out_tri(tmp_tri),
      .out(tmp)
  );

  register #(
      .WIDTH(XLEN)
  ) instr_reg (
      .clk  (clk),
      .rst_n(rst_n),

      .wenable(write_instr),
      .oenable(1'b1),

      .in(bus),
      .out_tri(instr)
  );

  wire [XLEN-1:0] alu_out;

  alu #(
      .XLEN(XLEN)
  ) alu (
      .op_a   (act),
      .op_b   (tmp),
      .control(alu_control),

      .flags_in (flags),
      .flags_out(alu_flags_out),

      .oenable(read_alu),
      .out    (alu_out),
      .out_tri(alu_out_tri)
  );

  wire [2*XLEN-1:0] pc;
  wire [2*XLEN-1:0] regs_out_rp;

  register_array #(
      .XLEN(XLEN)
  ) reg_array (
      .clk  (clk),
      .rst_n(rst_n),

      .reg_sel(reg_sel),
      .wdata  (bus),
      .wenable(write_regs),
      .oenable(read_regs),
      .rdata  (regs_out),
      .rpdata (regs_out_rp),

      .swap_hl_de  (swap_hl_de),
      .cpy_hl_to_sp(cpy_hl_to_sp),

      .inc(inc_rp),
      .dec(dec_rp),
      .pc (pc)
  );

  wire is_sss_mem, is_sss_a, is_ddd_mem, is_ddd_a, is_alu_op_cmp;
  wire [2:0] sss, ddd, cc, alu_op;
  wire [1:0] rp;
  wire is_mov, is_sphl, is_mvi, is_lxi, is_lda, is_sta, is_lhld, is_shld, is_ldax, is_stax, is_xchg,
       is_alu_reg, is_alu_imm, is_alu_alt, is_inr, is_dcr, is_inx, is_dcx, is_dad, is_nop;

  instr_decoder #(
      .XLEN(XLEN)
  ) instr_decoder (
      .instr(instr),

      .is_sss_mem   (is_sss_mem),
      .is_sss_a     (is_sss_a),
      .is_ddd_mem   (is_ddd_mem),
      .is_ddd_a     (is_ddd_a),
      .is_alu_op_cmp(is_alu_op_cmp),

      .sss   (sss),
      .ddd   (ddd),
      .rp    (rp),
      .cc    (cc),
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
      .is_nop    (is_nop),
      .is_dad    (is_dad)
  );

  wire       write_adr;
  wire [3:0] reg_sel;
  wire [4:0] alu_control;
  wire       write;
  wire a_src, act_src, flags_src;

  wire data_in_enable, data_out_enable;
  wire write_data_out;
  wire swap_hl_de;
  wire cpy_hl_to_sp;

  wire inc_rp, dec_rp;

  control control (
      .clk  (clk),
      .rst_n(rst_n),

      .sss   (sss),
      .ddd   (ddd),
      .rp    (rp),
      .cc    (cc),
      .alu_op(alu_op),

      .is_sss_mem   (is_sss_mem),
      .is_sss_a     (is_sss_a),
      .is_ddd_mem   (is_ddd_mem),
      .is_ddd_a     (is_ddd_a),
      .is_alu_op_cmp(is_alu_op_cmp),

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
      .is_nop    (is_nop),

      .ready(ready),
      .sync (sync),

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

      .swap_hl_de  (swap_hl_de),
      .cpy_hl_to_sp(cpy_hl_to_sp),

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
      .oenable(1'b1),

      .in(regs_out_rp),
      .out_tri(addr)
  );

  data_bus_buffer #(
      .XLEN(XLEN)
  ) data_bus_buffer (
      .clk  (clk),
      .rst_n(rst_n),

      .bus        (bus),
      .out_wenable(write_data_out),
      .out_enable (data_out_enable),
      .in_enable  (data_in_enable),

      .out(data)
  );

  assign write_n = ~write;
endmodule
