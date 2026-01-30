`default_nettype none `timescale 1ns / 1ps

`include "i8080.vh"

module decimal_adjust #(
    parameter XLEN = 8
) (
    input wire [XLEN-1:0] in,
    input wire [XLEN-1:0] flags_in,

    output wire [XLEN-1:0] out,
    output reg  [XLEN-1:0] flags_out
);
  reg [XLEN/2-1:0] d0, d1;

  always @(*) begin
    {d1, d0}  = in;
    flags_out = flags_in;

    if (d0 > 9 || flags_out[`FA]) begin
      {d1, d0} = {d1, d0} + 6;
      flags_out[`FA] = 1;
    end

    if (d1 > 9 || flags_in[`FC]) begin
      d1             = d1 + 6;
      flags_out[`FC] = 1;
    end
  end

  assign out = {d1, d0};
endmodule
