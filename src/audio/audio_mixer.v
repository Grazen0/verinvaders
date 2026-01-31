`default_nettype none `timescale 1ns / 1ps

module audio_mixer #(
    parameter WIDTH = 8,
    parameter N = 2
) (
    input wire [WIDTH-1:0] channel_1,
    input wire [WIDTH-1:0] channel_2,
    input wire [WIDTH-1:0] channel_3,
    input wire [WIDTH-1:0] channel_4,
    input wire [WIDTH-1:0] channel_5,
    input wire [WIDTH-1:0] channel_6,
    input wire [WIDTH-1:0] channel_7,
    input wire [WIDTH-1:0] channel_8,

    output wire [WIDTH-N+2:0] out
);
  wire [WIDTH+2:0] sum = channel_1 + channel_2 + channel_3 + channel_4 +
                         channel_5 + channel_6 + channel_7 + channel_8;

  assign out = sum >> N;
endmodule
