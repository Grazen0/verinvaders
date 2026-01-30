`default_nettype none `timescale 1ns / 1ps

module shift_register #(
    parameter XLEN = 8
) (
    input wire clk,
    input wire rst_n,

    input  wire [XLEN-1:0] wdata,
    input  wire            wenable,
    input  wire [     2:0] shift_amount,
    output wire [XLEN-1:0] shift_result
);
  wire [2*XLEN-1:0] shift_value;

  register shift_hi_reg (
      .clk  (clk),
      .rst_n(rst_n),

      .wenable(wenable),
      .in     (wdata),
      .out    (shift_value[15:8])
  );

  register shift_lo_reg (
      .clk  (clk),
      .rst_n(rst_n),

      .wenable(wenable),
      .in     (shift_value[15:8]),
      .out    (shift_value[XLEN-1:0])
  );

  wire [2*XLEN-1:0] shift_result_full = shift_value << shift_amount;
  assign shift_result = shift_result_full[15:8];
endmodule

