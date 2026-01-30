`default_nettype none `timescale 1ns / 1ps

`include "i8080.vh"
`include "compat.vh"
`include "nes_bridge.vh"

`define INP0_SELF_TEST 0

`define INP1_COIN 0
`define INP1_P2START 1
`define INP1_P1START 2
`define INP1_P1SHOOT 4
`define INP1_P1LEFT 5
`define INP1_P1RIGHT 6

`define INP2_SHIPS 1:0
`define INP2_TILT 2
`define INP2_EXTRA_SHIP 3
`define INP2_P2SHOOT 4
`define INP2_P2LEFT 5
`define INP2_P2RIGHT 6
`define INP2_COIN_INFO 7

module invaders (
    input wire clk,
    input wire vga_clk,
    input wire rst_n,

    input wire [4:0] dip,

    output wire joypad_scl_out,
    input  wire joypad_sda_in,
    output wire joypad_sda_out,

    output wire [3:0] vga_red,
    output wire [3:0] vga_green,
    output wire [3:0] vga_blue,
    output wire h_sync,
    output wire v_sync
);
  localparam XLEN = 8;
  localparam ZZ = {XLEN{1'bz}};

  wire [2*XLEN-1:0] addr;
  wire [XLEN-1:0] io_addr = addr[XLEN-1:0];
  tri [XLEN-1:0] data;

  wire sync;
  wire dbin;
  wire write_n;

  localparam ROM_SIZE = 8 * 1024;  // 8 KB
  localparam RAM_SIZE = 8 * 1024;  // 8 KB

  wire data_sel = addr[2*XLEN-1-:3] == 3'b000;
  wire mem_enable = ~in_out & ~inta;

  rom #(
      .SOURCE_FILE(`MK_PATH("../", "build/invaders.mem")),
      .SIZE(ROM_SIZE),
      .DATA_WIDTH(XLEN)
  ) rom (
      .addr(addr[$clog2(ROM_SIZE)-1:0]),
      .data(data),

      .enable (mem_enable & data_sel),
      .oenable(dbin)
  );

  ram #(
      .SIZE(RAM_SIZE),
      .DATA_WIDTH(XLEN)
  ) ram (
      .clk(clk),

      .addr_1(addr[$clog2(RAM_SIZE)-1:0]),
      .data_1(data),

      .enable_1 (mem_enable & ~data_sel),
      .oenable_1(dbin),
      .wenable_1(~write_n),

      .addr_2(video_ram_addr),
      .data_2(video_ram_data)
  );

  wire iint = mid_screen | vblank;

  i8080 #(
      .XLEN(XLEN)
  ) i8080 (
      .clk  (clk),
      .rst_n(rst_n),

      .ready(1'b1),
      .iint (iint),

      .wwait(),
      .inte (),

      .sync   (sync),
      .dbin   (dbin),
      .write_n(write_n),

      .addr(addr),
      .data(data)
  );

  wire [XLEN-1:0] status;

  register status_reg (
      .clk  (clk),
      .rst_n(rst_n),

      .wenable(sync),

      .in (data),
      .out(status)
  );

  wire inta = status[`STATUS_INTA];
  wire in_out = status[`STATUS_INP] | status[`STATUS_OUT];

  wire read_io = dbin & status[`STATUS_INP];
  wire write_io = ~write_n & status[`STATUS_OUT];

  localparam SHIFT_AMOUNT_WIDTH = 3;

  wire [SHIFT_AMOUNT_WIDTH-1:0] shift_amount;

  register #(
      .WIDTH(SHIFT_AMOUNT_WIDTH)
  ) shift_amount_reg (
      .clk  (clk),
      .rst_n(rst_n),

      .wenable(write_io & (io_addr <= 8'h02)),

      .in (data[SHIFT_AMOUNT_WIDTH-1:0]),
      .out(shift_amount)
  );

  wire [XLEN-1:0] shift_result;

  shift_register #(
      .XLEN(XLEN)
  ) shift_register (
      .clk  (clk),
      .rst_n(rst_n),

      .wdata       (data),
      .wenable     (write_io & (io_addr == 8'h04)),
      .shift_amount(shift_amount),
      .shift_result(shift_result)
  );


  reg [XLEN-1:0] input_0;
  reg [XLEN-1:0] input_1;
  reg [XLEN-1:0] input_2;

  always @(*) begin
    // See https://computerarcheology.com/Arcade/SpaceInvaders/Hardware.html
    // for info on the input bits.
    input_0                   = 8'b0000_1110;
    input_1                   = 8'b0000_1000;
    input_2                   = 8'b0000_0000;

    input_0[`INP0_SELF_TEST]  = dip[4];  // self-test request (?)

    // it's nicer when the coin is detected on button press instead of release
    input_1[`INP1_COIN]       = ~joypad_data[`JOYP_SELECT];
    input_1[`INP1_P1START]    = joypad_data[`JOYP_START];
    input_1[`INP1_P1SHOOT]    = joypad_data[`JOYP_A];
    input_1[`INP1_P1LEFT]     = joypad_data[`JOYP_LEFT];
    input_1[`INP1_P1RIGHT]    = joypad_data[`JOYP_RIGHT];

    input_2[`INP2_SHIPS]      = dip[3:2];  // ships
    input_2[`INP2_EXTRA_SHIP] = dip[1];  // extra ship at 1500 or 1000 pts
    input_2[`INP2_COIN_INFO]  = dip[0];  // show coin info
  end

  assign data = (read_io && io_addr == 8'h00) ? input_0 : ZZ;
  assign data = (read_io && io_addr == 8'h01) ? input_1 : ZZ;
  assign data = (read_io && io_addr == 8'h02) ? input_2 : ZZ;
  assign data = (read_io && io_addr == 8'h03) ? shift_result : ZZ;

  wire [$clog2(RAM_SIZE)-1:0] video_ram_addr;
  wire [XLEN-1:0] video_ram_data;

  wire mid_screen;
  wire vblank;

  wire [XLEN-1:0] rst_instr = {2'b11, mid_screen ? 3'b001 : 3'b010, 3'b111};

  assign data = (dbin & inta) ? rst_instr : ZZ;

  video_unit #(
      .RAM_SIZE(RAM_SIZE),
      .XLEN(XLEN)
  ) video_unit (
      .clk  (vga_clk),
      .rst_n(rst_n),

      .ram_addr(video_ram_addr),
      .ram_data(video_ram_data),

      .vga_red  (vga_red),
      .vga_green(vga_green),
      .vga_blue (vga_blue),
      .h_sync   (h_sync),
      .v_sync   (v_sync),

      .mid_screen(mid_screen),
      .vblank    (vblank)
  );

  wire       nes_ready;
  wire [7:0] joypad;
  wire       joypad_valid;

  nes_bridge #(
      .SCL_PERIOD(20)  // 100 KHz assuming 2 MHz clk
  ) nes_bridge (
      .clk  (clk),
      .rst_n(rst_n),

      .ready       (nes_ready),
      .start       (nes_ready),
      .joypad      (joypad),
      .joypad_valid(joypad_valid),

      .scl_out(joypad_scl_out),
      .sda_in (joypad_sda_in),
      .sda_out(joypad_sda_out)
  );

  wire [7:0] joypad_data;

  register #(
      .RESET_VALUE(8'h00)
  ) joypad_reg (
      .clk  (clk),
      .rst_n(rst_n),

      .wenable(joypad_valid),

      .in (~joypad),
      .out(joypad_data)
  );
endmodule
