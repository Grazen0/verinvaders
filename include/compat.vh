`ifndef INVADERS_COMPAT_VH
`define INVADERS_COMPAT_VH

// Vivado uses paths relative to the current file, but Icarus Verilog uses
// paths relative to the current working directory. This macro aims to provide
// a way to make paths that work on both Vivado and iverilog.
`ifdef IVERILOG
`define MK_PATH(prefix, path) path
`else
`define MK_PATH(prefix, path) {prefix, path}
`endif

`endif
