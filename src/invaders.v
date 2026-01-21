`default_nettype none `timescale 1ns / 1ps

module invaders (
    input wire clk,
    input wire rst_n
);
  wire [15:0] addr;
  tri  [ 7:0] data;

  rom #(
      .SOURCE_FILE("/home/jdgt/Code/verilog/verinvaders/data/invaders"),
      .ADDR_WIDTH(13)  // 8 KB
  ) rom (
      .addr(addr),
      .oenable(1'b0),

      .out(data)
  );

  i8080 i8080 (
      .clk  (clk),
      .rst_n(rst_n),

      .hold (1'b0),
      .ready(1'b1),
      .iint (1'b0),

      .addr(addr),
      .data(data)
  );
endmodule
