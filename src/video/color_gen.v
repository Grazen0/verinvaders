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
      pos_color = 12'hF66;  // red
    end else if (x_pos_virt < 72
        && (x_pos_virt >= 15 || (y_pos_virt >= 16 && y_pos_virt <= 134))) begin
      pos_color = 12'h6F6;  // green
    end else begin
      pos_color = 12'hFFF;  // white
    end
  end

  assign ram_addr = 'h400 + ('h20 * y_pos_virt) + x_pos_virt[$clog2(V_FRAME)-2:3];

  wire pixel_on = ram_data[x_pos_virt[2:0]];

  wire visible = (x_pos_virt < FRAME_WIDTH) & (y_pos_virt < FRAME_HEIGHT);
  assign color = pos_color & {12{pixel_on & visible}};
endmodule

