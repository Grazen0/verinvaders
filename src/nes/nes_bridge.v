`default_nettype none `timescale 1ns / 1ps

`include "nes_bridge.vh"
`include "nes/i2c_controller.vh"

module nes_bridge #(
    parameter SLAVE_ADDR = 7'h52,
    parameter SCL_PERIOD = 500  // 10 Khz assuming base clock of 50 MHz
) (
    input wire clk,
    input wire rst,

    input wire start,

    output wire scl_out,
    input  wire sda_in,
    output wire sda_out,

    output wire ready,
    output reg [7:0] joypad,
    output reg joypad_valid
);
  localparam S_IDLE = 4'd0;
  localparam S_START_1 = 4'd1;
  localparam S_ADDRW_WRITE = 4'd2;
  localparam S_INIT_WRITE = 4'd3;
  localparam S_STOP_1 = 4'd4;
  localparam S_START_2 = 4'd5;
  localparam S_ADDRR_WRITE = 4'd6;
  localparam S_DATA_READ = 4'd7;
  localparam S_STOP_2 = 4'd8;

  localparam RW_WRITE = 1'b0;
  localparam RW_READ = 1'b1;

  reg [3:0] state, state_next;
  reg [2:0] read_ctr, read_ctr_next;

  reg  [1:0] i2c_cmd;
  reg        i2c_start;
  reg  [7:0] i2c_wdata;
  reg        i2c_wack;
  reg  [7:0] joypad_next;
  reg        joypad_valid_next;

  wire       i2c_ready;
  wire [7:0] i2c_rdata;

  i2c_controller #(
      .SCL_PERIOD(SCL_PERIOD)
  ) i2c (
      .clk  (clk),
      .rst(rst),

      .cmd  (i2c_cmd),
      .start(i2c_start),
      .wdata(i2c_wdata),
      .wack (i2c_wack),

      .ready(i2c_ready),
      .rdata(i2c_rdata),

      .scl_out(scl_out),
      .sda_in (sda_in),
      .sda_out(sda_out)
  );

  always @(*) begin
    i2c_start         = 0;
    i2c_cmd           = 2'bxx;
    i2c_wdata         = 8'bxxxx_xxxx;
    i2c_wack          = 1'bx;
    read_ctr_next     = read_ctr;
    joypad_next       = joypad;
    joypad_valid_next = joypad_valid;

    state_next        = state;

    case (state)
      S_IDLE: begin
        if (start) begin
          i2c_start         = 1;
          i2c_cmd           = `CMD_START;
          state_next        = S_START_1;
          joypad_valid_next = 0;
        end
      end
      S_START_1: begin
        if (i2c_ready) begin
          i2c_start  = 1;
          i2c_cmd    = `CMD_WRITE;
          i2c_wdata  = {SLAVE_ADDR, RW_WRITE};
          state_next = S_ADDRW_WRITE;
        end
      end
      S_ADDRW_WRITE: begin
        if (i2c_ready) begin
          i2c_start  = 1;
          i2c_cmd    = `CMD_WRITE;
          i2c_wdata  = 8'h00;
          state_next = S_INIT_WRITE;
        end
      end
      S_INIT_WRITE: begin
        if (i2c_ready) begin
          i2c_start  = 1;
          i2c_cmd    = `CMD_STOP;
          state_next = S_STOP_1;
        end
      end
      S_STOP_1: begin
        if (i2c_ready) begin
          i2c_start  = 1;
          i2c_cmd    = `CMD_START;
          state_next = S_START_2;
        end
      end
      S_START_2: begin
        if (i2c_ready) begin
          i2c_start  = 1;
          i2c_cmd    = `CMD_WRITE;
          i2c_wdata  = {SLAVE_ADDR, RW_READ};
          state_next = S_ADDRR_WRITE;
        end
      end
      S_ADDRR_WRITE: begin
        if (i2c_ready) begin
          i2c_start     = 1;
          i2c_cmd       = `CMD_READ;
          state_next    = S_DATA_READ;
          i2c_wack      = 0;
          read_ctr_next = 6;
        end
      end
      S_DATA_READ: begin
        if (i2c_ready) begin
          read_ctr_next = read_ctr - 1;

          i2c_start     = 1;
          i2c_cmd       = `CMD_READ;
          state_next    = S_DATA_READ;
          i2c_wack      = read_ctr_next == 1;

          if (read_ctr_next == 1) begin
            // Just read penultimate byte
            joypad_next[`JOYP_RIGHT]  = i2c_rdata[7];
            joypad_next[`JOYP_DOWN]   = i2c_rdata[6];
            joypad_next[`JOYP_SELECT] = i2c_rdata[4];
            joypad_next[`JOYP_START]  = i2c_rdata[2];
          end else if (read_ctr_next == 0) begin
            // Just read last byte
            joypad_next[`JOYP_B]    = i2c_rdata[6];
            joypad_next[`JOYP_A]    = i2c_rdata[4];
            joypad_next[`JOYP_LEFT] = i2c_rdata[1];
            joypad_next[`JOYP_UP]   = i2c_rdata[0];
            joypad_valid_next       = 1;

            i2c_start               = 1;
            i2c_cmd                 = `CMD_STOP;
            state_next              = S_STOP_2;
          end
        end
      end
      S_STOP_2: begin
        if (i2c_ready) begin
          state_next = S_IDLE;
        end
      end
      default: begin
        state_next = S_IDLE;
      end
    endcase
  end

  always @(posedge clk) begin
    if (rst) begin
      read_ctr     <= 0;
      joypad       <= 8'h00;
      joypad_valid <= 0;
      state        <= S_IDLE;
    end else begin
      read_ctr     <= read_ctr_next;
      joypad       <= joypad_next;
      joypad_valid <= joypad_valid_next;
      state        <= state_next;
    end
  end

  assign ready = state == S_IDLE;
endmodule
