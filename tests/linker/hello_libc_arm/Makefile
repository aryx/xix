# Build/run the hello_libc closure fixture with xix's own o5a/o5l,
# straight from the frozen closure.tgz -- no goken checkout needed.
# See ../../../test-hello-libc-arm.sh for the same pipeline run as a
# pass/fail regression test, and closure.tgz's own header comment (in
# ../../../test-hello-libc-arm.sh) for how to regenerate the archive.
#
#   make        -- build hello.out
#   make run    -- build (if needed), then run it under qemu-arm
#   make clean  -- remove build artifacts (never touches closure.tgz)
#
# Requires xix's own o5a/o5l already built (dune build, from the repo
# root) -- this Makefile is a convenience wrapper around
# scripts/build-c-program.py, not a replacement for the repo's real
# build systems (dune/omk, see the top-level CLAUDE.md).

XIX_ROOT      := $(shell cd ../../.. && pwd)
CLOSURE_TGZ   := closure.tgz
CLOSURE_SUBDIR:= hello_libc_closure_5l
BUILD_DIR     := .build
BINARY        := hello.out

.PHONY: all run clean

all: $(BINARY)

$(BUILD_DIR)/$(CLOSURE_SUBDIR): $(CLOSURE_TGZ)
	rm -rf $(BUILD_DIR)
	mkdir -p $(BUILD_DIR)
	tar xzf $(CLOSURE_TGZ) -C $(BUILD_DIR)

$(BINARY): $(BUILD_DIR)/$(CLOSURE_SUBDIR)
	python3 $(XIX_ROOT)/scripts/build-c-program.py 5 $(BUILD_DIR)/$(CLOSURE_SUBDIR) _main \
		--out $(BINARY) --verbose

run: $(BINARY)
	qemu-arm ./$(BINARY)

clean:
	rm -rf $(BUILD_DIR) $(BINARY)
