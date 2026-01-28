
`default_nettype none `timescale 1ns / 1ps

module invaders_tb ();
  reg clk, rst_n;
  always #5 clk = ~clk;

  invaders invaders (
      .clk  (clk),
      .rst_n(rst_n)
  );

  always @(posedge clk) begin
    if (invaders.i8080.control.mcycle == 0 && invaders.i8080.control.tstate == 0) begin  // M1
      $display("pc = %h, sp = %h, hl = %h, a = %h, f = %b, mem = %h", invaders.i8080.adr_reg.data,
               invaders.i8080.reg_array.sp, {invaders.i8080.reg_array.h, invaders.i8080.reg_array.l
               }, invaders.i8080.a, invaders.i8080.flags_reg.data, {invaders.ram.mem[16'h7FF],
                                                                    invaders.ram.mem[16'h7FE]});
    end
  end

  initial begin
    $dumpvars(0, invaders_tb);

    $display("                                  SZ A P C");

    clk   = 0;
    rst_n = 0;

    #10 rst_n = 1;

    #100500 $finish();
  end
endmodule
