`default_nettype none `timescale 1ns / 1ps

module edge_detector #(
    parameter WIDTH   = 1,
    parameter NEGEDGE = 0
) (
    input wire clk,
    input wire rst_n,

    input wire enable,
    input wire [WIDTH-1:0] in,
    output reg [WIDTH-1:0] out
);
  reg [WIDTH-1:0] in_prev;

  always @(posedge clk) begin
    if (!rst_n) begin
      out     <= {WIDTH{1'b0}};
      in_prev <= {WIDTH{NEGEDGE ? 1'b1 : 1'b0}};
    end else if (enable) begin
      out     <= NEGEDGE ? (~in & in_prev) : (in & ~in_prev);
      in_prev <= in;
    end
  end
endmodule

