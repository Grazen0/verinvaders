`default_nettype none `timescale 1ns / 1ps

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

  localparam FRAME_HEIGHT = 256;
  localparam FRAME_WIDTH = 224;

  reg [$clog2(V_FRAME)-1:0] y_pos, y_pos_next;
  reg [$clog2(H_LINE)-1:0] x_pos, x_pos_next;
  reg h_visible, h_visible_next;
  reg v_visible, v_visible_next;
  reg h_sync_next;
  reg v_sync_next;

  always @(*) begin
    y_pos_next = y_pos;
    x_pos_next = x_pos + 1;

    h_visible_next = h_visible;
    v_visible_next = v_visible;

    if (x_pos_next == H_FRONT) begin
      h_visible_next = 0;
    end else if (x_pos_next == H_LINE) begin
      // Next line
      h_visible_next = 1;
      x_pos_next     = 0;
      y_pos_next     = y_pos + 1;

      if (y_pos_next == V_FRONT) begin
        v_visible_next = 0;
      end else if (y_pos_next == V_FRAME) begin
        // Next frame
        v_visible_next = 1;
        y_pos_next     = 0;
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
      x_pos     <= 0;
      y_pos     <= 0;
      h_sync    <= 1;
      v_sync    <= 1;
      h_visible <= 1;
      v_visible <= 1;
    end else begin
      x_pos     <= x_pos_next;
      y_pos     <= y_pos_next;
      h_sync    <= h_sync_next;
      v_sync    <= v_sync_next;
      h_visible <= h_visible_next;
      v_visible <= v_visible_next;
    end
  end

  assign ram_addr = 'h41F + ('h20 * x_pos) - y_pos[$clog2(V_FRAME)-1:3];

  wire cur_color = ram_data[7-y_pos[2:0]];

  wire visible = h_visible & v_visible & (x_pos < FRAME_WIDTH) & (y_pos < FRAME_HEIGHT);
  assign {vga_red, vga_green, vga_blue} = {12{cur_color & visible}};

  assign mid_screen = y_pos == HEIGHT / 2;
  assign vblank = y_pos == V_BACK;
endmodule
