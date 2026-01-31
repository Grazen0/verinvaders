`default_nettype none `timescale 1ns / 1ps

`include "compat.vh"

module top (
    input wire clk,
    input wire rst,

    input wire [4:0] dip,

    output tri joypad_scl_pin,
    inout  tri joypad_sda_pin,

    output wire [3:0] vga_red,
    output wire [3:0] vga_green,
    output wire [3:0] vga_blue,
    output wire h_sync,
    output wire v_sync,

    output wire audio_out
);
  wire sys_clk;
  wire vga_clk;

  clk_divider #(
      .PERIOD(50)
  ) sys_divider (
      .clk_in (clk),
      .clk_out(sys_clk)
  );

  clk_divider #(
      .PERIOD(4)
  ) vga_divider (
      .clk_in (clk),
      .clk_out(vga_clk)
  );

  wire joypad_scl_out;
  wire joypad_sda_in;
  wire joypad_sda_out;

  invaders invaders (
      .clk      (sys_clk),
      .vga_clk  (vga_clk),
      .audio_clk(clk),
      .rst_n    (~rst),

      .dip(dip),

      .joypad_scl_out(joypad_scl_out),
      .joypad_sda_in (joypad_sda_in),
      .joypad_sda_out(joypad_sda_out),

      .vga_red  (vga_red),
      .vga_green(vga_green),
      .vga_blue (vga_blue),
      .h_sync   (h_sync),
      .v_sync   (v_sync),

      .audio_out(audio_out)
  );

  assign joypad_scl_pin = ~joypad_scl_out ? 1'b0 : 1'bz;
  assign joypad_sda_pin = ~joypad_sda_out ? 1'b0 : 1'bz;
  assign joypad_sda_in  = joypad_sda_pin;
endmodule
