`default_nettype none `timescale 1ns / 1ps

`include "i8080.vh"

`define INPUT1_COIN 8'b0000_0001
`define INPUT1_P1START 8'b0000_0100
`define INPUT1_P1SHOOT 8'b0001_0000
`define INPUT1_P1LEFT 8'b0010_0000
`define INPUT1_P1RIGHT 8'b0100_0000

module cpm (
    input wire clk,
    input wire rst_n
);
  localparam XLEN = 8;

  wire [2*XLEN-1:0] addr;
  wire [XLEN-1:0] io_addr = addr[7:0];
  tri [XLEN-1:0] data;

  wire sync;
  wire dbin;
  wire write_n;

  localparam RAM_SIZE = 8 * 1024;  // 8 KB

  ram #(
      .SOURCE_FILE("/home/jdgt/Code/verilog/verinvaders/tests/build/microcosm_diag.mem"),
      .SIZE(RAM_SIZE),
      .DATA_WIDTH(XLEN)
  ) ram (
      .clk(clk),

      .addr_1(addr[$clog2(RAM_SIZE)-1:0]),
      .data_1(data),

      .enable_1 (~in_out & ~inta),
      .oenable_1(dbin),
      .wenable_1(~write_n),

      .addr_2({$clog2(RAM_SIZE) {1'bx}})
  );

  i8080 #(
      .XLEN(XLEN)
  ) i8080 (
      .clk  (clk),
      .rst_n(rst_n),

      .hold (1'b0),
      .ready(1'b1),
      .iint (1'b0),

      .sync   (sync),
      .dbin   (dbin),
      .write_n(write_n),

      .addr(addr),
      .data(data)
  );

  wire [XLEN-1:0] status;

  register status_reg (
      .clk  (clk),
      .rst_n(rst_n),

      .wenable(sync),
      .oenable(1'b1),

      .in (data),
      .out(status)
  );

  wire inta = status[`STATUS_INTA];
  wire in_out = status[`STATUS_INP] | status[`STATUS_OUT];
endmodule











