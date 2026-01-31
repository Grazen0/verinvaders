`default_nettype none `timescale 1ns / 1ps

module clk_divider #(
    parameter PERIOD = 2
) (
    input  wire clk_in,
    output wire clk_out
);
  localparam HALF_PERIOD = PERIOD / 2;

  reg [$clog2(HALF_PERIOD)-1:0] counter;
  reg clk_out_raw;

  always @(posedge clk_in) begin
    if (counter >= HALF_PERIOD - 1) begin
      clk_out_raw <= ~clk_out_raw;
      counter     <= 0;
    end else begin
      counter <= counter + 1;
    end
  end

  initial clk_out_raw = 0;
  initial counter = 0;

`ifdef IVERILOG
  assign clk_out = clk_out_raw;
`else
  BUFG bufg (
      .I(clk_out_raw),
      .O(clk_out)
  );
`endif
endmodule
