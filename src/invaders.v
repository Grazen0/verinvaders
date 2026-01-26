`default_nettype none `timescale 1ns / 1ps

module invaders (
    input wire clk,
    input wire rst_n
);
  wire [15:0] addr;
  tri [7:0] data;

  wire sync;
  wire dbin;
  wire write_n;

  localparam ROM_ADDR_WIDTH = 13;  // 8 KB
  localparam RAM_ADDR_WIDTH = 13;  // 8 KB

  wire data_sel = addr[15:13] == 3'b000;

  rom #(
      .SOURCE_FILE("/home/jdgt/Code/verilog/verinvaders/tests/build/main.mem"),
      .ADDR_WIDTH (ROM_ADDR_WIDTH)
  ) rom (
      .addr(addr[ROM_ADDR_WIDTH-1:0]),
      .data(data),

      .enable (data_sel),
      .oenable(dbin)
  );

  ram #(
      .ADDR_WIDTH(RAM_ADDR_WIDTH)
  ) ram (
      .clk(clk),

      .addr(addr[RAM_ADDR_WIDTH-1:0]),
      .data(data),

      .enable (~data_sel),
      .oenable(dbin),
      .wenable(~write_n)
  );

  i8080 i8080 (
      .clk  (clk),
      .rst_n(rst_n),

      .hold (1'b0),
      .ready(1'b1),
      .iint (1'b0),

      .sync(sync),
      .dbin(dbin),
      .write_n(write_n),

      .addr(addr),
      .data(data)
  );

  wire [7:0] status;

  register status_reg (
      .clk  (clk),
      .rst_n(rst_n),

      .wenable(sync),
      .oenable(1'b1),

      .in (data),
      .out(status)
  );
endmodule
