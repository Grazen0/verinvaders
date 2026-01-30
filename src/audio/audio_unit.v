`default_nettype none `timescale 1ns / 1ps


module pwm_generator #(
    parameter BIT_WIDTH = 8
) (
    input wire clk,
    input wire rst_n,

    input wire [BIT_WIDTH:0] duty,
    output wire out
);
  reg [BIT_WIDTH-1:0] counter;

  always @(posedge clk) begin
    if (!rst_n) begin
      counter <= 0;
    end else begin
      counter <= counter + 1;
    end
  end

  assign out = counter < duty;
endmodule

module audio_unit (
    input wire clk,
    input wire rst_n
);
endmodule
