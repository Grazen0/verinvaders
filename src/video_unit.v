`default_nettype none `timescale 1ns / 1ps

module color_gen #(
    parameter SCALE = 1,

    parameter WIDTH = 0,
    parameter HEIGHT = 0,
    parameter FRAME_WIDTH = 0,
    parameter FRAME_HEIGHT = 0,

    parameter V_FRAME = 0,
    parameter H_LINE  = 0,

    parameter RAM_SIZE = 8 * 1024,  // 8K
    parameter RAM_ADDR_WIDTH = $clog2(RAM_SIZE),
    parameter XLEN = 8
) (
    input wire [ $clog2(H_LINE)-1:0] x_pos,
    input wire [$clog2(V_FRAME)-1:0] y_pos,

    output wire [RAM_ADDR_WIDTH-1:0] ram_addr,
    input wire [XLEN-1:0] ram_data,

    output wire [11:0] color
);
  localparam H_EXCESS = WIDTH - (SCALE * FRAME_WIDTH);
  localparam V_EXCESS = HEIGHT - (SCALE * FRAME_HEIGHT);

  reg [11:0] pos_color;

  wire [$clog2(V_FRAME / SCALE)-1:0] y_pos_virt = (y_pos / SCALE) - (V_EXCESS / (2 * SCALE));
  wire [$clog2(H_LINE / SCALE)-1:0] x_pos_virt = (x_pos / SCALE) - (H_EXCESS / (2 * SCALE));

  always @(*) begin
    if (x_pos_virt >= 192 && x_pos_virt < 224) begin
      pos_color = 12'hF44;  // red
    end else if (x_pos_virt < 72
        && (x_pos_virt >= 15 || (y_pos_virt >= 16 && y_pos_virt <= 134))) begin
      pos_color = 12'h4F4;  // green
    end else begin
      pos_color = 12'hFFF;  // white
    end
  end

  assign ram_addr = 'h400 + ('h20 * y_pos_virt) + x_pos_virt[$clog2(V_FRAME)-2:3];

  wire pixel_on = ram_data[x_pos_virt[2:0]];

  wire visible = (x_pos_virt < FRAME_WIDTH) & (y_pos_virt < FRAME_HEIGHT);
  assign color = pos_color & {12{pixel_on & visible}};
endmodule

module video_unit #(
    parameter RAM_SIZE = 8 * 1024,  // 8K
    parameter RAM_ADDR_WIDTH = $clog2(RAM_SIZE),
    parameter XLEN = 8
) (
    input wire clk,
    input wire rst_n,

    output wire [RAM_ADDR_WIDTH-1:0] ram_addr,
    input wire [XLEN-1:0] ram_data,

    output wire [3:0] vga_red,
    output wire [3:0] vga_green,
    output wire [3:0] vga_blue,
    output reg h_sync,
    output reg v_sync,

    output wire mid_screen,
    output wire vblank
);
  localparam WIDTH = 640;
  localparam HEIGHT = 480;

  localparam H_VISIBLE = 0;
  localparam H_FRONT = H_VISIBLE + WIDTH;
  localparam H_SYNC = H_FRONT + 16;
  localparam H_BACK = H_SYNC + 96;
  localparam H_LINE = H_BACK + 48;

  localparam V_VISIBLE = 0;
  localparam V_FRONT = V_VISIBLE + HEIGHT;
  localparam V_SYNC = V_FRONT + 10;
  localparam V_BACK = V_SYNC + 2;
  localparam V_FRAME = V_BACK + 33;

  localparam FRAME_HEIGHT = 224;
  localparam FRAME_WIDTH = 256;

  reg [$clog2(V_FRAME)-1:0] y_pos, y_pos_next;
  reg [$clog2(H_LINE)-1:0] x_pos, x_pos_next;
  reg h_sync_next;
  reg v_sync_next;

  reg [11:0] color;
  wire [11:0] color_next;

  always @(*) begin
    y_pos_next = y_pos;
    x_pos_next = x_pos + 1;

    if (x_pos_next == H_LINE) begin
      // Next line
      x_pos_next = 0;
      y_pos_next = y_pos + 1;

      if (y_pos_next == V_FRAME) begin
        // Next frame
        y_pos_next = 0;
      end
    end

    case (x_pos_next)
      H_SYNC:  h_sync_next = 0;
      H_BACK:  h_sync_next = 1;
      default: h_sync_next = h_sync;
    endcase

    case (y_pos_next)
      V_SYNC:  v_sync_next = 0;
      V_BACK:  v_sync_next = 1;
      default: v_sync_next = v_sync;
    endcase
  end

  always @(posedge clk) begin
    if (!rst_n) begin
      x_pos  <= 0;
      y_pos  <= 0;
      h_sync <= 1;
      v_sync <= 1;
      color  <= 12'h000;
    end else begin
      x_pos  <= x_pos_next;
      y_pos  <= y_pos_next;
      h_sync <= h_sync_next;
      v_sync <= v_sync_next;
      color  <= color_next;
    end
  end

  color_gen #(
      .SCALE         (2),
      .WIDTH         (WIDTH),
      .HEIGHT        (HEIGHT),
      .FRAME_WIDTH   (FRAME_WIDTH),
      .FRAME_HEIGHT  (FRAME_HEIGHT),
      .V_FRAME       (V_FRAME),
      .H_LINE        (H_LINE),
      .RAM_SIZE      (RAM_SIZE),
      .RAM_ADDR_WIDTH(RAM_ADDR_WIDTH),
      .XLEN          (XLEN)
  ) color_gen (
      .x_pos(x_pos_next),
      .y_pos(y_pos_next),

      .ram_addr(ram_addr),
      .ram_data(ram_data),

      .color(color_next)
  );

  assign mid_screen = y_pos == HEIGHT / 2;
  assign vblank = y_pos == V_BACK;

  assign {vga_red, vga_green, vga_blue} = color;
endmodule
