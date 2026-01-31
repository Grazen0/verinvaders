`default_nettype none `timescale 1ns / 1ps `default_nettype none `timescale 1ns / 1ps

module square_wave_generator #(
    parameter PERIOD_WIDTH = 32
) (
    input wire clk,
    input wire rst,

    input  wire [PERIOD_WIDTH-1:0] period,
    output wire                    out
);
  wire [PERIOD_WIDTH-1:0] cnt;

  counter #(
      .WIDTH(PERIOD_WIDTH)
  ) counter (
      .clk(clk),
      .rst(rst),

      .enable(1'b1),
      .cmp   (period),
      .out   (cnt)
  );

  assign out = cnt < (period / 2);
endmodule

