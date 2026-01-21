
`default_nettype none `timescale 1ns / 1ps

module invaders_tb ();
  reg clk, rst_n;
  always #5 clk = ~clk;

  invaders invaders (
      .clk  (clk),
      .rst_n(rst_n)
  );

  initial begin
    $dumpvars(0, invaders_tb);

    clk   = 0;
    rst_n = 0;

    #10 rst_n = 1;

    #100 $finish();
  end
endmodule
