`ifndef INVADERS_I8080_VH
`define INVADERS_I8080_VH

`define FC 0
`define FP 2
`define FA 4 // TODO: implement this flag
`define FZ 6
`define FS 7

`define FLAGS_SRC_ALU 1'b0
`define FLAGS_SRC_BUS 1'b1

`define A_SRC_ALU 1'b0
`define A_SRC_BUS 1'b1

`define ACT_SRC_A 1'b0
`define ACT_SRC_BUS 1'b1

`define RP_SEL_BC 3'b000
`define RP_SEL_DE 3'b001
`define RP_SEL_HL 3'b010
`define RP_SEL_SP 3'b011
`define RP_SEL_WZ 3'b100
`define RP_SEL_PC 3'b101

`define RP_LO 1'b1
`define RP_HI 1'b0

`define REG_SEL_B {`RP_SEL_BC, `RP_HI}
`define REG_SEL_C {`RP_SEL_BC, `RP_LO}
`define REG_SEL_D {`RP_SEL_DE, `RP_HI}
`define REG_SEL_E {`RP_SEL_DE, `RP_LO}
`define REG_SEL_H {`RP_SEL_HL, `RP_HI}
`define REG_SEL_L {`RP_SEL_HL, `RP_LO}
`define REG_SEL_W {`RP_SEL_WZ, `RP_HI}
`define REG_SEL_Z {`RP_SEL_WZ, `RP_LO}
`define REG_SEL_SP_HI {`RP_SEL_SP, `RP_HI}
`define REG_SEL_SP_LO {`RP_SEL_SP, `RP_LO}
`define REG_SEL_PC_HI {`RP_SEL_PC, `RP_HI}
`define REG_SEL_PC_LO {`RP_SEL_PC, `RP_LO}

`define STATUS_INTA 0
`define STATUS_WO_N 1
`define STATUS_STACK 2
`define STATUS_HLTA 3
`define STATUS_OUT 4
`define STATUS_M1 5
`define STATUS_INP 6
`define STATUS_MEMR 7

`endif
