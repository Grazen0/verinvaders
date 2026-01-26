`default_nettype none `timescale 1ns / 1ps

module rom #(
    parameter SOURCE_FILE = "",
    parameter DATA_WIDTH  = 8,
    parameter ADDR_WIDTH  = 0
) (
    input  wire [ADDR_WIDTH-1:0] addr,
    output tri  [DATA_WIDTH-1:0] data,

    input wire enable,
    input wire oenable
);
  localparam SIZE = 2 ** ADDR_WIDTH;

  reg [DATA_WIDTH-1:0] mem[0:SIZE-1];

  assign data = (enable & oenable) ? mem[addr] : {DATA_WIDTH{1'bz}};

  initial begin
    $readmemh(SOURCE_FILE, mem);
  end
endmodule
