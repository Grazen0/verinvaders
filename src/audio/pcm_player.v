`default_nettype none `timescale 1ns / 1ps

module pcm_player #(
    parameter SOURCE_FILE = "",
    parameter SAMPLES_SIZE = 0,
    parameter SAMPLE_WIDTH = 8,
    parameter CLK_FREQ = 0,
    parameter SAMPLE_FREQ = 8000,  // 8 KHz
    parameter LOOP = 0
) (
    input wire clk,
    input wire rst,

    input wire play,
    input wire stop,
    output wire [SAMPLE_WIDTH-1:0] out
);
  reg [SAMPLE_WIDTH-1:0] samples[0:SAMPLES_SIZE-1];
  reg [$clog2(SAMPLES_SIZE)-1:0] sc, sc_next;

  wire counter_tc;

  counter counter (
      .clk(clk),
      .rst(rst),

      .enable(playing),
      .cmp   (CLK_FREQ / SAMPLE_FREQ),
      .tc    (counter_tc),
      .out   ()
  );

  reg playing, playing_next;

  always @(*) begin
    sc_next      = sc;
    playing_next = playing;

    if (stop) begin
      playing_next = 0;
      sc_next      = 0;
    end else if (play) begin
      playing_next = 1;
      sc_next      = 0;
    end else if (playing && counter_tc) begin
      sc_next = sc_next + 1;

      if (sc_next == SAMPLES_SIZE) begin
        sc_next = 0;

        if (!LOOP) begin
          playing_next = 0;
        end
      end

    end
  end

  always @(posedge clk) begin
    if (rst) begin
      playing <= 0;
      sc      <= 0;
    end else begin
      playing <= playing_next;
      sc      <= sc_next;
    end
  end

  integer i;

  initial begin
    for (i = 0; i < SAMPLES_SIZE; i = i + 1) begin
      samples[i] = 0;
    end

    $readmemh(SOURCE_FILE, samples);
  end

  assign out = playing ? samples[sc] : 0;
endmodule

