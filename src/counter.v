`default_nettype none `timescale 1ns / 1ps

module counter #(
    parameter WIDTH = 32,
    parameter RESET_VALUE = {WIDTH{1'b0}}
) (
    input wire clk,
    input wire rst,

    input wire enable,
    input wire [WIDTH-1:0] cmp,
    output wire tc,
    output reg [WIDTH-1:0] out
);
  always @(posedge clk) begin
    if (rst) begin
      out <= RESET_VALUE;
    end else if (enable) begin
      if (tc) out <= 0;
      else out <= out + 1;
    end
  end

  assign tc = out >= cmp - 1;
endmodule
