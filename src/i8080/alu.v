`default_nettype none `timescale 1ns / 1ps

`include "i8080.vh"

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

