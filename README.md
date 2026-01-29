# VerInvaders

A clone of Space Invaders in Verilog.

The CPU is currently finished and passes the 8080/8085 CPU Diagnostic by
Microcosm Associates (`tests/microcosm_diag.asm`), but the complete game does
not yet work on an FPGA.

The CPU is in dire need of refactoring with microcode due currently having a
gargantuan control unit of ~900 lines of hardwired control logic.
