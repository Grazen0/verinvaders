`default_nettype none `timescale 1ns / 1ps

module ram #(
    parameter SOURCE_FILE = "",
    parameter DATA_WIDTH = 8,
    parameter SIZE = 0,
    parameter ADDR_WIDTH = $clog2(SIZE)
) (
    input wire clk,

    input  wire [ADDR_WIDTH-1:0] addr_1,
    output tri  [DATA_WIDTH-1:0] data_1,

    input wire enable_1,
    input wire oenable_1,
    input wire wenable_1,

    input  wire [ADDR_WIDTH-1:0] addr_2,
    output wire [DATA_WIDTH-1:0] data_2
);
  reg [DATA_WIDTH-1:0] mem[0:SIZE-1];

  always @(posedge clk) begin
    if (enable_1 && wenable_1) begin
      mem[addr_1] <= data_1;
    end
  end

  assign data_1 = (enable_1 && oenable_1) ? mem[addr_1] : {DATA_WIDTH{1'bz}};
  assign data_2 = mem[addr_2];

  initial begin
    if (SOURCE_FILE != "") begin
      $readmemh(SOURCE_FILE, mem);
    end
  end
endmodule
