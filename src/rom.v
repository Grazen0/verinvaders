`default_nettype none `timescale 1ns / 1ps

module rom #(
    parameter SOURCE_FILE = "",
    parameter DATA_WIDTH = 8,
    parameter SIZE = 0,
    parameter ADDR_WIDTH = $clog2(SIZE)
) (
    input wire [ADDR_WIDTH-1:0] addr,
    inout tri  [DATA_WIDTH-1:0] data,

    input wire enable,
    input wire oenable
);
  reg [DATA_WIDTH-1:0] mem[0:SIZE-1];

  assign data = (enable & oenable) ? mem[addr] : {DATA_WIDTH{1'bz}};

  initial begin
    $readmemh(SOURCE_FILE, mem);
  end
endmodule
