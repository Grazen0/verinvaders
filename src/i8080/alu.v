`default_nettype none `timescale 1ns / 1ps

`include "i8080.vh"

module bit_adder (
    input wire a,
    input wire b,
    input wire c_in,
    input wire sub,

    output wire s,
    output wire c_out
);
  wire a_c = a ^ sub;

  assign s = a ^ b ^ c_in;
  assign c_out = (a_c & b) | (a_c & c_in) | (b & c_in);
endmodule

module adder #(
    parameter XLEN = 8
) (
    input wire [XLEN-1:0] a,
    input wire [XLEN-1:0] b,
    input wire c_in,
    input wire sub,

    output wire [XLEN-1:0] result,
    output wire c_aux,
    output wire c_out
);
  wire [XLEN:0] carries;

  genvar i;

  generate
    for (i = 0; i < XLEN; i = i + 1) begin : g_adder
      bit_adder bit_adder (
          .a   (a[i]),
          .b   (b[i]),
          .c_in(carries[i]),
          .sub (sub),

          .s    (result[i]),
          .c_out(carries[i+1])
      );
    end
  endgenerate

  assign carries[0] = c_in;
  assign c_out      = carries[XLEN];
  assign c_aux      = carries[XLEN/2];
endmodule

module alu #(
    parameter XLEN = 8
) (
    input wire [XLEN-1:0] op_a,
    input wire [XLEN-1:0] op_b,
    input wire [     4:0] control,
    input wire [XLEN-1:0] flags_in,

    output reg [XLEN-1:0] out,
    output reg [XLEN-1:0] flags_out
);
  wire [XLEN-1:0] sum;
  wire            sum_carry;
  wire            sum_carry_aux;

  adder #(
      .XLEN(XLEN)
  ) adder (
      .op_a(op_a),
      .op_b(op_b),
      .c_in(~control[2] & control[0] & flags_in[`FC]),
      .sub (control[1]),

      .result(sum),
      .c_out (sum_carry),
      .c_aux (sum_carry_aux)
  );

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

  reg set_pzs_flags;

  always @(*) begin
    flags_out     = flags_in;
    set_pzs_flags = 0;

    casez (control)
      5'b00_0??, 5'b00_111: begin  // add/adc/sbb/sbc
        out            = sum;
        flags_out[`FC] = sum_carry;
        flags_out[`FA] = sum_carry_aux;
        set_pzs_flags  = 1;
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

      5'b10_??0: begin  // inr
        out           = op_b + 1;
        set_pzs_flags = 1;
      end
      5'b10_??1: begin  // dcr
        out           = op_b - 1;
        set_pzs_flags = 1;
      end

      5'b11_???: begin  // add/sub but only set carry, used for dad
        out = sum;
        flags_out[`FC] = sum_carry;
      end

      default: out = {XLEN{1'bx}};
    endcase

    if (set_pzs_flags) begin
      flags_out[`FP] = ~(^out);
      flags_out[`FZ] = out == 0;
      flags_out[`FS] = out[XLEN-1];
    end
  end
endmodule

