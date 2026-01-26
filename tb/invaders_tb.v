
`default_nettype none `timescale 1ns / 1ps

module invaders_tb ();
  reg clk, rst_n;
  always #5 clk = ~clk;

  invaders invaders (
      .clk  (clk),
      .rst_n(rst_n)
  );

  always @(invaders.i8080.control.mcycle) begin
    if (invaders.i8080.control.mcycle == 0) begin  // M1
      $display("pc = %h, bc = %h, a = %h, f = %b", invaders.i8080.reg_array.pc, {
               invaders.i8080.reg_array.b, invaders.i8080.reg_array.c}, invaders.i8080.a,
               invaders.i8080.flags_reg.data);
    end
  end

  initial begin
    $dumpvars(0, invaders_tb);

    $display("                       SZ A P C");

    clk   = 0;
    rst_n = 0;

    #10 rst_n = 1;

    #1500 $finish();
  end
endmodule
