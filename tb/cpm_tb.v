`default_nettype none `timescale 1ns / 1ps

module cpm_tb ();
  reg clk, rst;
  always #5 clk = ~clk;

  cpm cpm (
      .clk(clk),
      .rst(rst)
  );

  wire [15:0] pc = cpm.i8080.adr_reg.data;
  wire [15:0] sp = cpm.i8080.reg_array.sp;
  wire [7:0] b = cpm.i8080.reg_array.b;
  wire [7:0] c = cpm.i8080.reg_array.c;
  wire [7:0] d = cpm.i8080.reg_array.d;
  wire [7:0] e = cpm.i8080.reg_array.e;
  wire [7:0] h = cpm.i8080.reg_array.h;
  wire [7:0] l = cpm.i8080.reg_array.l;
  wire [7:0] a = cpm.i8080.a_reg.data;
  wire [7:0] f = cpm.i8080.flags_reg.data;

  integer step;
  integer i;

  always @(posedge clk) begin
    if (cpm.i8080.control.mcycle == 0 && cpm.i8080.control.tstate == 0) begin  // M1
      if (step != 0) begin
        // $display("pc = %h, a = %h, f = %b, hl = %h", pc, a, f, {h, l});

        if (pc == 16'h069F) begin
          $display("CPUER");
          $finish();
        end
      end

      if (pc == 16'h0005) begin
        case (c)
          9: begin
            for (i = {d, e}; cpm.ram.mem[i] != "$"; i = i + 1) begin
              $write("%c", cpm.ram.mem[i]);
              $fflush();
            end
          end
          2: begin
            $write("%c", e);
            $fflush();
          end
          default: $display("invalid BIOS call (c = 0x%h)", c);
        endcase
      end

      step = step + 1;
    end
  end

  initial begin
    $dumpvars(0, cpm_tb);

    step = 0;

    clk  = 1;
    rst  = 1;

    #500 rst = 0;
  end
endmodule
