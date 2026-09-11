#!/bin/bash
# Differential test driver for the RISC-V (RV32) port. Mirrors
# test-arm.sh/test-mips.sh -- see
# docs/claude_notes/notes_riscv_port_plan.txt and
# docs/claude_notes/todo_riscv_port.org.
#
# Usage: ./test-riscv.sh

set -e

cd "$(dirname "$0")"

# file:entry_symbol pairs -- entry defaults to _main (the linkers'
# default) when not TEXT _start.
CASES=(
    "tests/linker/riscv_diff/hello_linux_riscv.s:_start"
)

FAIL=0
for c in "${CASES[@]}"; do
    file=${c%%:*}
    entry=${c##*:}
    echo "### $file (entry $entry)"
    if ! ./scripts/diff-riscv.sh "$file" "$entry"; then
        FAIL=1
    fi
    echo
done

if [ "$FAIL" -ne 0 ]; then
    echo "test-riscv.sh: some comparisons failed" 1>&2
    exit 1
fi
