`default_nettype none `timescale 1ns / 1ps

`define FC 0
`define FP 2
`define FAC 4
`define FZ 6
`define FS 7

`define FLAGS_SRC_ALU 1'b0
`define FLAGS_SRC_BUS 1'b1

`define RP_SEL_BC 3'b000
`define RP_SEL_DE 3'b001
`define RP_SEL_HL 3'b010
`define RP_SEL_SP 3'b011
`define RP_SEL_WZ 3'b100
`define RP_SEL_PC 3'b101

`define RP_LO 1'b0
`define RP_HI 1'b1

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

      .in (bus),
      .out(out)
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

    input wire is_sss_mem,
    input wire is_sss_a,
    input wire is_ddd_mem,
    input wire is_ddd_a,

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

    input  wire ready,
    output wire sync,

    output reg data_in_enable,
    output reg data_out_enable,
    output reg write_data_out,
    output reg flags_src,

    output wire write,
    output wire dbin,

    output reg read_flags,
    output reg write_flags,
    output reg write_acc,
    output reg read_acc,
    output reg write_acc_latch,
    output reg read_tmp,
    output reg write_tmp,
    output reg write_instr,
    output reg read_alu,
    output reg read_regs,
    output reg write_regs,

    output reg swap_hl_de,
    output reg cpy_hl_to_sp,

    output reg [3:0] reg_sel,
    output reg [3:0] alu_control,
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

  wire [3:0] sss_ext = {1'b0, sss};
  wire [3:0] ddd_ext = {1'b0, ddd};
  wire [2:0] rp_ext = {1'b0, rp};

  always @(*) begin
    inc_rp          = 0;
    dec_rp          = 0;

    data_in_enable  = 0;
    data_out_enable = 0;
    write_data_out  = 0;

    flags_src       = `FLAGS_SRC_ALU;

    reg_sel         = 4'bxxxx;
    alu_control     = 3'bxxx;

    read_flags      = 0;
    write_flags     = 0;
    write_acc       = 0;
    read_acc        = 0;
    write_acc_latch = 0;
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
          if (is_sss_a) begin
            read_acc = 1;
          end else begin
            reg_sel   = sss_ext;
            read_regs = 1;
          end

          write_tmp = 1;
        end

        if (is_sphl) begin
          cpy_hl_to_sp = 1;
        end

        if (is_xchg) begin
          swap_hl_de = 1;
        end
      end
      {M1, T4}: begin
        if (is_mov) begin
          if (!is_sss_mem && !is_ddd_mem) begin
            read_tmp = 1;

            if (is_ddd_a) begin
              write_acc = 1;
            end else begin
              reg_sel = ddd_ext;
              write_regs = 1;
            end
          end else begin
            mcycle_end = 1;

            reg_sel = {`RP_SEL_HL, 1'bx};
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

        if (is_xchg) begin
          instr_end = 1;
        end
      end
      {M1, T5}: begin
        mcycle_end = 1;

        if (is_mov || is_sphl) begin
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

        if (is_mvi || is_lxi || is_lda || is_sta || is_lhld || is_shld) begin
          reg_sel = {`RP_SEL_PC, 1'bx};
          inc_rp = 1;
        end

        if (is_stax) begin
          read_acc = 1;
          write_data_out = 1;
        end
      end

      {M2, T2}: begin
        if (is_mov) begin
          if (is_sss_mem) begin
            data_in_enable = 1;
            write_regs = 1;
            reg_sel = ddd_ext;
          end else begin
            data_out_enable = 1;
          end
        end

        if (is_mvi) begin
          data_in_enable = 1;

          if (is_ddd_mem) begin
            write_tmp = 1;
          end else if (is_ddd_a) begin
            write_acc = 1;
          end else begin
            write_regs = 1;
            reg_sel    = ddd_ext;
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
          write_acc      = 1;
        end

        if (is_stax) begin
          data_out_enable = 1;
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
      end
      {M3, T2}: begin
        if (is_mvi) begin
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
      end
      {M3, T3}: begin
        mcycle_end = 1; // No instruction has an M3 cycle over 3 T-states long

        if (is_mvi || is_lxi) begin
          instr_end = 1;
        end

        if (is_lda || is_sta || is_lhld || is_shld) begin
          reg_sel = {`RP_SEL_WZ, 1'bx};
          write_adr = 1;
          inc_rp = 1; // for lhld and shld
        end
      end

      {M4, T1}: begin
        if (is_sta) begin
          read_acc       = 1;
          write_data_out = 1;
        end

        if (is_shld) begin
          reg_sel = `REG_SEL_L;
          read_regs = 1;
          write_data_out = 1;
        end
      end

      {M4, T2}: begin
        if (is_lda) begin
          data_in_enable = 1;
          write_acc      = 1;
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

      default: ;
    endcase
    // verilog_format: on

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
    output wire [2:0] alu_control,

    output wire is_sss_mem,
    output wire is_sss_a,
    output wire is_ddd_mem,
    output wire is_ddd_a,

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
    output reg is_xchg
);
  localparam REG_MEM = 3'b110;
  localparam REG_A = 3'b111;

  always @(*) begin
    is_mov  = 0;
    is_sphl = 0;
    is_mvi  = 0;
    is_lxi  = 0;
    is_lda  = 0;
    is_sta  = 0;
    is_lhld = 0;
    is_shld = 0;
    is_ldax = 0;
    is_stax = 0;
    is_xchg = 0;

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
      default:       ;
    endcase
  end

  assign sss = instr[2:0];
  assign ddd = instr[5:3];
  assign rp = instr[5:4];
  assign cc = instr[5:3];
  assign alu_control = instr[5:3];

  assign is_sss_mem = sss == REG_MEM;
  assign is_sss_a = sss == REG_A;
  assign is_ddd_mem = ddd == REG_MEM;
  assign is_ddd_a = ddd == REG_A;
endmodule

module alu #(
    parameter XLEN = 8
) (
    input wire [XLEN-1:0] op_a,
    input wire [XLEN-1:0] op_b,
    input wire [3:0] control,
    input wire oenable,

    output tri [XLEN-1:0] out,

    input  wire [XLEN-1:0] flags_in,
    output reg  [XLEN-1:0] flags_out
);

  reg [XLEN-1:0] result;
  reg carry;

  wire [XLEN:0] op_a_ext = {1'b0, op_a};
  wire [XLEN:0] op_b_ext = {1'b0, op_b};
  wire [XLEN:0] carry_ext = {{(XLEN) {1'b0}}, control[0] & flags_in[`FC]};

  wire [XLEN:0] op_b_ext_signed = control[1] ? -op_b_ext : op_b_ext;
  wire [XLEN:0] carry_ext_signed = control[1] ? -carry_ext : carry_ext;

  always @(*) begin
    carry     = 0;
    flags_out = flags_in;

    // TODO: use A flag
    casez (control)
      4'b00zz: begin
        {flags_out[`FC], result} = op_a_ext + op_b_ext_signed + carry_ext_signed;
        flags_out[`FP]           = ~(^result);
        flags_out[`FZ]           = result == 0;
        flags_out[`FS]           = result[XLEN-1];
      end
      4'b0100: begin
        result         = op_a & op_b;
        flags_out[`FC] = 0;
        flags_out[`FP] = ~(^result);
        flags_out[`FZ] = result == 0;
        flags_out[`FS] = result[XLEN-1];
      end
      4'b0101: begin
        result         = op_a ^ op_b;
        flags_out[`FC] = 0;
        flags_out[`FP] = ~(^result);
        flags_out[`FZ] = result == 0;
        flags_out[`FS] = result[XLEN-1];
      end
      4'b0110: begin
        result         = op_a | op_b;
        flags_out[`FC] = 0;
        flags_out[`FP] = ~(^result);
        flags_out[`FZ] = result == 0;
        flags_out[`FS] = result[XLEN-1];
      end
      default: result = {XLEN{1'bx}};
    endcase
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

  tri [XLEN-1:0] flags;
  tri [XLEN-1:0] acc;
  tri [XLEN-1:0] acc_latch;
  tri [XLEN-1:0] tmp;
  tri [XLEN-1:0] instr;
  tri [XLEN-1:0] alu_flags_out;
  tri [XLEN-1:0] alu_out;
  tri [XLEN-1:0] regs_out;

  assign bus = acc;
  assign bus = tmp;
  assign bus = flags;
  assign bus = alu_out;
  assign bus = regs_out;

  wire read_flags, write_flags;
  wire read_acc, write_acc;
  wire write_acc_latch;
  wire read_tmp, write_tmp;
  wire write_instr;
  wire read_alu;
  wire read_regs, write_regs;

  register #(
      .WIDTH(XLEN),
      .RESET_VALUE(8'b0000_0010)
  ) flags_reg (
      .clk  (clk),
      .rst_n(rst_n),

      .wenable(write_flags),
      .oenable(read_flags),

      .in (flags_src == `FLAGS_SRC_ALU ? alu_flags_out : bus),
      .out(flags)
  );

  register #(
      .WIDTH(XLEN)
  ) acc_reg (
      .clk  (clk),
      .rst_n(rst_n),

      .wenable(write_acc),
      .oenable(read_acc),

      .in (bus),
      .out(acc)
  );

  register #(
      .WIDTH(XLEN)
  ) acc_latch_reg (
      .clk  (clk),
      .rst_n(rst_n),

      .wenable(write_acc_latch),
      .oenable(1'b1),

      .in (acc),
      .out(acc_latch)
  );

  register #(
      .WIDTH(XLEN)
  ) tmp_reg (
      .clk  (clk),
      .rst_n(rst_n),

      .wenable(write_tmp),
      .oenable(read_tmp),

      .in (bus),
      .out(tmp)
  );

  register #(
      .WIDTH(XLEN)
  ) instr_reg (
      .clk  (clk),
      .rst_n(rst_n),

      .wenable(write_instr),
      .oenable(1'b1),

      .in (bus),
      .out(instr)
  );


  alu #(
      .XLEN(XLEN)
  ) alu (
      .op_a(tmp),
      .op_b(acc_latch),
      .control(alu_control),

      .flags_in (flags),
      .flags_out(alu_flags_out),

      .oenable(read_alu),
      .out(alu_out)
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

  wire is_sss_mem, is_sss_a, is_ddd_mem, is_ddd_a;
  wire [2:0] sss, ddd, cc;
  wire [1:0] rp;
  wire is_mov, is_sphl, is_mvi, is_lxi, is_lda, is_sta, is_lhld, is_shld, is_ldax, is_stax, is_xchg;

  instr_decoder #(
      .XLEN(XLEN)
  ) instr_decoder (
      .instr(instr),

      .is_sss_mem(is_sss_mem),
      .is_sss_a  (is_sss_a),
      .is_ddd_mem(is_ddd_mem),
      .is_ddd_a  (is_ddd_a),

      .sss(sss),
      .ddd(ddd),
      .rp (rp),
      .cc (cc),

      .is_mov (is_mov),
      .is_sphl(is_sphl),
      .is_mvi (is_mvi),
      .is_lxi (is_lxi),
      .is_lda (is_lda),
      .is_sta (is_sta),
      .is_lhld(is_lhld),
      .is_shld(is_shld),
      .is_ldax(is_ldax),
      .is_stax(is_stax),
      .is_xchg(is_xchg)
  );

  wire write_adr;
  wire [3:0] reg_sel;
  wire [3:0] alu_control;
  wire write;
  wire flags_src;

  wire data_in_enable, data_out_enable;
  wire write_data_out;
  wire swap_hl_de;
  wire cpy_hl_to_sp;

  wire inc_rp, dec_rp;

  control control (
      .clk  (clk),
      .rst_n(rst_n),

      .sss(sss),
      .ddd(ddd),
      .rp (rp),
      .cc (cc),

      .is_sss_mem(is_sss_mem),
      .is_sss_a  (is_sss_a),
      .is_ddd_mem(is_ddd_mem),
      .is_ddd_a  (is_ddd_a),

      .is_mov (is_mov),
      .is_sphl(is_sphl),
      .is_mvi (is_mvi),
      .is_lxi (is_lxi),
      .is_lda (is_lda),
      .is_sta (is_sta),
      .is_lhld(is_lhld),
      .is_shld(is_shld),
      .is_ldax(is_ldax),
      .is_stax(is_stax),
      .is_xchg(is_xchg),

      .ready(ready),
      .sync (sync),

      .data_in_enable (data_in_enable),
      .data_out_enable(data_out_enable),
      .write_data_out (write_data_out),

      .read_flags     (read_flags),
      .write_flags    (write_flags),
      .read_acc       (read_acc),
      .write_acc      (write_acc),
      .write_acc_latch(write_acc_latch),
      .read_tmp       (read_tmp),
      .write_tmp      (write_tmp),
      .write_instr    (write_instr),
      .read_regs      (read_regs),
      .write_regs     (write_regs),

      .swap_hl_de  (swap_hl_de),
      .cpy_hl_to_sp(cpy_hl_to_sp),

      .reg_sel    (reg_sel),
      .alu_control(alu_control),
      .write_adr  (write_adr),
      .inc_rp     (inc_rp),
      .dec_rp     (dec_rp),
      .flags_src  (flags_src),

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

      .in (regs_out_rp),
      .out(addr)
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
