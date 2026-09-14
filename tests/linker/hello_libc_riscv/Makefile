# Build/run/test the hello_libc closure fixture with xix's own
# oia/oil, straight from the frozen closure.tgz -- no goken checkout
# needed. See test.sh's own header comment for how to regenerate the
# archive after a real lib_core/libc change.
#
#   make        -- build hello.oil.out (oil = xix's own linker -- see
#                  BINARY's own comment below for why this isn't just
#                  called hello.out)
#   make run    -- build (if needed), then run it under qemu-riscv32
#   make test   -- run test.sh (the pass/fail regression check)
#   make clean  -- remove build artifacts (never touches closure.tgz)
#
# Requires xix's own oia/oil already built (dune build, from the repo
# root) -- this Makefile is a convenience wrapper around
# scripts/build-c-program.py, not a replacement for the repo's real
# build systems (dune/omk, see the top-level CLAUDE.md).

XIX_ROOT      := $(shell cd ../../.. && pwd)
CLOSURE_TGZ   := closure.tgz
CLOSURE_SUBDIR:= hello_libc_closure_il
BUILD_DIR     := .build
# claude: named hello.oil.out, not just hello.out -- same reasoning as
# hello_libc_amd64/'s own hello.o6l.out (see its own comment): this is
# always the output from xix's own oil, never goken's, and
# build-c-program.py separately builds an *informational* goken-
# linked binary too (in a throwaway temp dir, never written here).
BINARY        := hello.oil.out

.PHONY: all run test clean

all: $(BINARY)

$(BUILD_DIR)/$(CLOSURE_SUBDIR): $(CLOSURE_TGZ)
	rm -rf $(BUILD_DIR)
	mkdir -p $(BUILD_DIR)
	tar xzf $(CLOSURE_TGZ) -C $(BUILD_DIR)

$(BINARY): $(BUILD_DIR)/$(CLOSURE_SUBDIR)
	python3 $(XIX_ROOT)/scripts/build-c-program.py i $(BUILD_DIR)/$(CLOSURE_SUBDIR) _main \
		--out $(BINARY) --verbose

run: $(BINARY)
	qemu-riscv32 ./$(BINARY)

test:
	./test.sh

clean:
	rm -rf $(BUILD_DIR) $(BINARY)
