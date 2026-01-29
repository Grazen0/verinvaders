`default_nettype none `timescale 1ns / 1ps

`include "i8080.vh"

`define INPUT1_COIN 8'b0000_0001
`define INPUT1_P1START 8'b0000_0100
`define INPUT1_P1SHOOT 8'b0001_0000
`define INPUT1_P1LEFT 8'b0010_0000
`define INPUT1_P1RIGHT 8'b0100_0000

module shift_register #(
    parameter XLEN = 8
) (
    input wire clk,
    input wire rst_n,

    input wire [XLEN-1:0] wdata,
    input wire wenable,
    input wire [2:0] shift_amount,

    input wire oenable,
    inout tri [XLEN-1:0] shift_result
);
  wire [2*XLEN-1:0] shift_value;

  register shift_hi_reg (
      .clk  (clk),
      .rst_n(rst_n),

      .wenable(wenable),
      .oenable(1'b1),

      .in (wdata),
      .out(shift_value[15:8])
  );

  register shift_lo_reg (
      .clk  (clk),
      .rst_n(rst_n),

      .wenable(wenable),
      .oenable(1'b1),

      .in (shift_value[15:8]),
      .out(shift_value[XLEN-1:0])
  );

  wire [2*XLEN-1:0] shift_result_full = shift_value << shift_amount;
  assign shift_result = oenable ? shift_result_full[15:8] : {XLEN{1'bz}};
endmodule

module invaders (
    input wire clk,
    input wire vga_clk,
    input wire rst_n,

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
      // .SOURCE_FILE("/home/jdgt/Code/verilog/verinvaders/tests/build/main.mem"),
      .SOURCE_FILE("/home/jdgt/Code/verilog/verinvaders/build/invaders.mem"),
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
      .oenable(1'b1),

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
      .oenable(1'b1),

      .in (data[SHIFT_AMOUNT_WIDTH-1:0]),
      .out(shift_amount)
  );

  shift_register #(
      .XLEN(XLEN)
  ) shift_register (
      .clk  (clk),
      .rst_n(rst_n),

      .wdata       (data),
      .wenable     (write_io & (io_addr == 8'h04)),
      .shift_amount(shift_amount),

      .oenable     (read_io & (io_addr == 8'h03)),
      .shift_result(data)
  );

  reg [XLEN-1:0] input_1;
  reg [XLEN-1:0] input_2;

  always @(*) begin
    input_1 = 8'b0000_0000;
    input_2 = 8'b0000_0000;

    input_1[0] = ~joypad_data[5];  // credit -> select
    input_1[2] = joypad_data[4];  // p1 start -> start
    input_1[4] = joypad_data[7];  // p1 shoot -> a
    input_1[5] = joypad_data[1];  // p1 left -> left
    input_1[6] = joypad_data[0];  // p1 right -> right
  end

  assign data = (read_io && io_addr == 8'h01) ? input_1 : {XLEN{1'bz}};
  assign data = (read_io && io_addr == 8'h02) ? input_2 : {XLEN{1'bz}};

  wire [$clog2(RAM_SIZE)-1:0] video_ram_addr;
  wire [XLEN-1:0] video_ram_data;

  wire mid_screen;
  wire vblank;

  wire [XLEN-1:0] rst_instr = {2'b11, mid_screen ? 3'b001 : 3'b010, 3'b111};

  assign data = (dbin & inta) ? rst_instr : {XLEN{1'bz}};

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
      .oenable(1'b1),

      .in (~joypad),
      .out(joypad_data)
  );
endmodule
