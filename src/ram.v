`default_nettype none `timescale 1ns / 1ps

module ram #(
    parameter DATA_WIDTH = 8,
    parameter ADDR_WIDTH = 0
) (
    input wire clk,

    input  wire [ADDR_WIDTH-1:0] addr,
    output tri  [DATA_WIDTH-1:0] data,

    input wire enable,
    input wire oenable,
    input wire wenable
);
  localparam SIZE = 2 ** ADDR_WIDTH;

  reg [DATA_WIDTH-1:0] mem[0:SIZE-1];

  always @(posedge clk) begin
    if (enable && wenable) begin
      mem[addr] <= data;
    end
  end

  assign data = (enable && oenable) ? mem[addr] : {DATA_WIDTH{1'bz}};
endmodule
