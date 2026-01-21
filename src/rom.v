`default_nettype none `timescale 1ns / 1ps

module rom #(
    parameter SOURCE_FILE = "",
    parameter DATA_WIDTH  = 8,
    parameter ADDR_WIDTH  = 0
) (
    input wire [ADDR_WIDTH-1:0] addr,
    input wire oenable,

    output tri [DATA_WIDTH-1:0] out
);
  localparam SIZE = 2 ** ADDR_WIDTH;

  reg [DATA_WIDTH-1:0] data[0:ADDR_WIDTH-1];

  assign out = oenable ? data[addr] : {DATA_WIDTH{1'bz}};

  initial begin
    $readmemh(SOURCE_FILE, data);
  end
endmodule
