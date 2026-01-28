BUILD_DIR := ./build
SRC_DIR := ./

SRCS = $(shell find $(SRC_DIR) -name '*.asm')
EXECS := $(patsubst %.asm,$(BUILD_DIR)/%,$(SRCS))
LISTS := $(patsubst %.asm,$(BUILD_DIR)/%.lst,$(SRCS))
MEMS := $(patsubst %.asm,$(BUILD_DIR)/%.mem,$(SRCS))

ZASM_FLAGS := --asm8080 -u -w

.PHONY: all clean

all: $(MEMS)

clean:
	rm -rf $(BUILD_DIR)

$(BUILD_DIR)/%.mem: $(BUILD_DIR)/%
	mkdir -p $(dir $@)
	xxd -p -c1 $< > $@


$(BUILD_DIR)/%: %.asm
	mkdir -p $(dir $@)
	zasm $(ZASM_FLAGS) -o $@ $<
