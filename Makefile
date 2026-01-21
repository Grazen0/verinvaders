BUILD_DIR := ./build

SRC_DIR := ./src
TB_DIR := ./tb

SRCS = $(shell find $(SRC_DIR) -name '*.v')
TBS = $(shell find $(TB_DIR) -name '*.v')

TARGETS := $(patsubst $(TB_DIR)/%.v,$(BUILD_DIR)/%.out,$(TBS))
VCD_DUMPS := $(patsubst $(TB_DIR)/%.v,$(BUILD_DIR)/%.vcd,$(TBS))

BIN_SRC := ./data/invaders
BIN_TARGET := invaders.mem

INC_DIRS := $(shell find ./include -type d)
INC_FLAGS := $(addprefix -I,$(INC_DIRS))

override IVERILOG_FLAGS += -DIVERILOG -Wall

.PHONY: all clean run wave

all: $(TARGETS)

clean:
	rm -rf $(BUILD_DIR)

$(BUILD_DIR)/$(BIN_TARGET): $(BIN_SRC)
	mkdir -p $(dir $@)
	xxd -p -c1 $< > $@

$(BUILD_DIR)/%.out: $(TB_DIR)/%.v $(SRCS) $(BUILD_DIR)/$(BIN_TARGET)
	mkdir -p $(dir $@)
	iverilog $(INC_FLAGS) $(IVERILOG_FLAGS) -o $@ $< $(SRCS) 

$(BUILD_DIR)/%.vcd: $(BUILD_DIR)/%.out
	mkdir -p $(dir $@)
	vvp $(VVP_FLAGS) $<
	mv dump.vcd $@

run: $(BUILD_DIR)/$(TB).out
	mkdir -p $(dir $(BUILD_DIR)/$(TB))
	vvp $(VVP_FLAGS) $<
	mv dump.vcd $(BUILD_DIR)/$(TB).vcd

wave: $(BUILD_DIR)/$(TB).vcd
	gtkwave $<
