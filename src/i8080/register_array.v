`default_nettype none `timescale 1ns / 1ps

`include "i8080.vh"

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

