`default_nettype none `timescale 1ns / 1ps

module top_tb ();
  reg clk, rst;
  always #5 clk = ~clk;

  top top (
      .clk(clk),
      .rst(rst),

      .dip(5'b00000)
  );

  wire [15:0] pc = top.invaders.i8080.adr_reg.data;
  wire [15:0] sp = top.invaders.i8080.reg_array.sp;
  wire [7:0] b = top.invaders.i8080.reg_array.b;
  wire [7:0] c = top.invaders.i8080.reg_array.c;
  wire [7:0] d = top.invaders.i8080.reg_array.d;
  wire [7:0] e = top.invaders.i8080.reg_array.e;
  wire [7:0] h = top.invaders.i8080.reg_array.h;
  wire [7:0] l = top.invaders.i8080.reg_array.l;
  wire [7:0] a = top.invaders.i8080.a_reg.data;
  wire [7:0] f = top.invaders.i8080.flags_reg.data;

  integer step = 0;

  always @(posedge top.sys_clk) begin
    if (top.invaders.i8080.control.mcycle == 0 && top.invaders.i8080.control.tstate == 0) begin  // M1
      $display("pc = %h, sp = %h, regs = %h %h %h %h %h %h %h %h", pc, sp, b, c, d, e, h, l, a, f);

      step = step + 1;
      if (step > 1000) $finish();
    end
  end

  initial begin
    $dumpvars(0, top_tb);

    clk = 1;
    rst = 1;

    #500 rst = 0;
  end
endmodule
