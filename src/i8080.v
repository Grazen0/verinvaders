`default_nettype none `timescale 1ns / 1ps

`define FC 0
`define FP 2
`define FAC 4
`define FZ 6
`define FS 7

module register #(
    parameter WIDTH = 8,
    parameter RESET_VALUE = 0
) (
    input wire clk,
    input wire rst_n,
    input wire enable,

    input  wire [WIDTH-1:0] in,
    output reg  [WIDTH-1:0] out
);
  always @(posedge clk or negedge rst_n) begin
    if (!rst_n) out <= RESET_VALUE;
    else if (enable) out <= in;
  end
endmodule

module control (
    input wire clk,
    input wire rst_n,

    input wire ready,

    output wire write_acc,
    output wire write_acc_latch,
    output wire write_tmp,
    output wire write_instr
);
  localparam T1 = 3'd0;
  localparam T2 = 3'd1;
  localparam TW = 3'd2;
  localparam T3 = 3'd3;
  localparam T4 = 3'd4;
  localparam T5 = 3'd5;

  reg [2:0] t_state, t_state_next;

  always @(*) begin
    case (t_state)
      T1: begin
        t_state_next = T2;
      end
      T2: begin
        t_state_next = ready ? T3 : TW;
      end
      TW: begin
        t_state_next = ready ? T3 : TW;
      end
      default: t_state_next = T1;
    endcase
  end

  always @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin
      t_state <= T1;
    end else begin
      t_state <= t_state_next;
    end
  end
endmodule

module alu (
    input wire [7:0] op_a,
    input wire [7:0] op_b,
    input wire [2:0] control,

    output wire [7:0] out,

    input  wire [7:0] flags_in,
    output wire [7:0] flags_out
);

endmodule

module register_file (
    input wire clk,
    input wire rst_n
);

endmodule

module i8080 (
    input wire clk,
    input wire rst_n,

    inout wire [ 7:0] data,
    input wire [15:0] addr,

    input  wire hold,
    output wire hlda,

    input  wire ready,
    output wire wwait,

    input  wire iint,
    output wire inte,

    output wire sync,

    output wire dbin,
    output wire write_n
);

  wire [7:0] flags;
  wire [7:0] acc;
  wire [7:0] acc_latch;
  wire [7:0] tmp;
  wire [7:0] instr;

  wire write_flags;
  wire write_acc;
  wire write_acc_latch;
  wire write_tmp;
  wire write_instr;

  register #(
      .RESET_VALUE(8'b0000_0010)
  ) flags_reg (
      .clk  (clk),
      .rst_n(rst_n),

      .enable(write_flags),
      .in    (alu_flags_out),
      .out   (flags)
  );


  register acc_reg (
      .clk  (clk),
      .rst_n(rst_n),

      .enable(write_acc),
      .out   (acc)
  );

  register acc_latch_reg (
      .clk  (clk),
      .rst_n(rst_n),

      .enable(write_acc_latch),
      .in    (acc),
      .out   (acc_latch)
  );

  register tmp_reg (
      .clk  (clk),
      .rst_n(rst_n),

      .enable(write_tmp),
      .out   (tmp)
  );

  register instr_reg (
      .clk  (clk),
      .rst_n(rst_n),

      .enable(write_instr),
      .out   (instr)
  );

  wire [7:0] alu_flags_out;

  alu alu (
      .op_a(tmp),
      .op_b(acc_latch),

      .flags_in (flags),
      .flags_out(alu_flags_out)
  );

  register reg_file (
      .clk  (clk),
      .rst_n(rst_n)
  );

  control control (
      .clk  (clk),
      .rst_n(rst_n)
  );
endmodule
