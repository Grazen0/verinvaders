`default_nettype none `timescale 1ns / 1ps

module counter #(
    parameter WIDTH = 32
) (
    input wire clk,
    input wire rst_n,

    input wire enable,
    input wire [WIDTH-1:0] cmp,
    output wire tc,
    output reg [WIDTH-1:0] out
);
  always @(posedge clk) begin
    if (!rst_n) begin
      out <= 0;
    end else if (enable) begin
      if (tc) out <= 0;
      else out <= out + 1;
    end
  end

  assign tc = out >= cmp - 1;
endmodule
