`default_nettype none `timescale 1ns / 1ps

module register #(
    parameter WIDTH = 8,
    parameter RESET_VALUE = {WIDTH{1'bx}}
) (
    input wire clk,
    input wire rst_n,

    input wire wenable,
    input wire oenable,

    input  wire [WIDTH-1:0] in,
    output tri  [WIDTH-1:0] out_tri,
    output wire [WIDTH-1:0] out
);
  reg [WIDTH-1:0] data;

  always @(posedge clk) begin
    if (!rst_n) data <= RESET_VALUE;
    else if (wenable) data <= in;
  end

  assign out_tri = oenable ? data : {WIDTH{1'bz}};
  assign out = data;
endmodule

