`default_nettype none `timescale 1ns / 1ps `default_nettype none `timescale 1ns / 1ps

`include "compat.vh"

module audio_unit #(
    parameter CLK_FREQ = 100_000_000
) (
    input wire clk,
    input wire rst,

    input wire [31:0] fleet_period,
    input wire        play_ufo,
    input wire        stop_ufo,
    input wire        play_shoot,
    input wire        play_player_hit,
    input wire        play_alien_hit,
    input wire        play_ufo_hit,

    output wire out
);
  localparam CHNL_WIDTH = 8;
  localparam SAMPLE_FREQ = 8000;

  wire fleet_square;
  wire [CHNL_WIDTH-1:0] ufo_chnl, shoot_chnl, player_hit_chnl, alien_hit_chnl, ufo_hit_chnl;
  wire [CHNL_WIDTH-1:0] fleet_chnl = fleet_square ? 8'hFF : 8'h00;

  square_wave_generator fleet_wave_generator (
      .clk  (clk),
      .rst(rst),

      .period(fleet_period),
      .out   (fleet_square)
  );

  pcm_player #(
      .SOURCE_FILE(`MK_PATH("../", "build/pcm/ufo.mem")),
      .SAMPLES_SIZE(1370),
      .CLK_FREQ(CLK_FREQ),
      .SAMPLE_FREQ(SAMPLE_FREQ),
      .LOOP(1)
  ) ufo_player (
      .clk  (clk),
      .rst(rst),

      .play(play_ufo),
      .stop(stop_ufo),
      .out (ufo_chnl)
  );

  pcm_player #(
      .SOURCE_FILE(`MK_PATH("../", "build/pcm/shoot.mem")),
      .SAMPLES_SIZE(2774),
      .CLK_FREQ(CLK_FREQ),
      .SAMPLE_FREQ(SAMPLE_FREQ)
  ) shoot_player (
      .clk  (clk),
      .rst(rst),

      .play(play_shoot),
      .stop(1'b0),
      .out (shoot_chnl)
  );

  pcm_player #(
      .SOURCE_FILE(`MK_PATH("../", "build/pcm/player_hit.mem")),
      .SAMPLES_SIZE(9755),
      .CLK_FREQ(CLK_FREQ),
      .SAMPLE_FREQ(SAMPLE_FREQ)
  ) player_hit_player (
      .clk  (clk),
      .rst(rst),

      .play(play_player_hit),
      .stop(1'b0),
      .out (player_hit_chnl)
  );

  pcm_player #(
      .SOURCE_FILE(`MK_PATH("../", "build/pcm/alien_hit.mem")),
      .SAMPLES_SIZE(2543),
      .CLK_FREQ(CLK_FREQ),
      .SAMPLE_FREQ(SAMPLE_FREQ)
  ) alien_hit_player (
      .clk  (clk),
      .rst(rst),

      .play(play_alien_hit),
      .stop(1'b0),
      .out (alien_hit_chnl)
  );

  pcm_player #(
      .SOURCE_FILE(`MK_PATH("../", "build/pcm/ufo_hit.mem")),
      .SAMPLES_SIZE(8728),
      .CLK_FREQ(CLK_FREQ),
      .SAMPLE_FREQ(SAMPLE_FREQ)
  ) ufo_hit_player (
      .clk  (clk),
      .rst(rst),

      .play(play_ufo_hit),
      .stop(1'b0),
      .out (ufo_hit_chnl)
  );

  localparam MIXER_N = 1;
  wire [CHNL_WIDTH-MIXER_N+2:0] pwm_duty;

  audio_mixer #(
      .WIDTH(CHNL_WIDTH),
      .N(MIXER_N)
  ) mixer (
      // 2 channels to give more volume to fleet
      .channel_1(fleet_chnl),
      .channel_2(fleet_chnl),
      .channel_3(ufo_chnl),
      .channel_4(shoot_chnl),
      .channel_5(player_hit_chnl),
      .channel_6(alien_hit_chnl),
      .channel_7(ufo_hit_chnl),
      .channel_8({CHNL_WIDTH{1'b0}}),

      .out(pwm_duty)
  );

  pwm_generator #(
      .DUTY_WIDTH(10)
  ) pwm_gen (
      .clk  (clk),
      .rst(rst),

      .duty(pwm_duty),
      .out (out)
  );
endmodule
