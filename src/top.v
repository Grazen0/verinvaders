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

  clk_divider #(
      .PERIOD(50)
  ) sys_divider (
      .clk_in (clk),
      .clk_out(sys_clk)
  );

  wire vga_clk;

`ifdef IVERILOG
  clk_divider #(
      .PERIOD(4)
  ) vga_divider (
      .clk_in (clk),
      .clk_out(vga_clk)
  );

`else
  wire vga_clkfb;

  MMCME2_BASE #(
      .CLKIN1_PERIOD(10.0),  // 100 MHz input
      .CLKFBOUT_MULT_F(8.0),
      .CLKOUT0_DIVIDE_F(32.0)  // 100 * 8 / 32 = 25 MHz
  ) vga_mmcm (
      .CLKIN1  (clk),
      .CLKFBIN (vga_clkfb),
      .CLKFBOUT(vga_clkfb),
      .CLKOUT0 (vga_clk),
      .LOCKED  ()
  );
`endif

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
