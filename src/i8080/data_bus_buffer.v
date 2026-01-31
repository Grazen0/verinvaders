`default_nettype none `timescale 1ns / 1ps

module data_bus_buffer #(
    parameter XLEN = 8
) (
    input wire clk,
    input wire rst,

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
      .rst(rst),

      .wenable(out_wenable),
      .in     (bus),
      .out    (out_data)
  );

  assign out = out_enable ? out_data : ZZ;
  assign out = sync ? status : ZZ;

  assign bus = in_enable ? out : ZZ;
endmodule
