#!/bin/bash
# Differential test driver for the RISC-V64 port (ojl, sharing
# Codegeni.ml/Rewritei.ml/Layouti.ml/Ast_asmi.ml with the RV32 port --
# goken's ja/jl are literally the same binaries as ia/il). Mirrors
# test-riscv.sh -- see docs/claude_notes/notes_riscv_port_plan.txt and
# docs/claude_notes/todo_riscv_port.org.
#
# Usage: ./test-riscv64.sh

set -e

cd "$(dirname "$0")"

# file:entry_symbol pairs -- entry defaults to _main (the linkers'
# default) when not TEXT _start.
CASES=(
    "tests/linker/riscv64_diff/hello_linux_riscv64.s:_start"
)

FAIL=0
for c in "${CASES[@]}"; do
    file=${c%%:*}
    entry=${c##*:}
    echo "### $file (entry $entry)"
    if ! ./scripts/diff-riscv64.sh "$file" "$entry"; then
        FAIL=1
    fi
    echo
done

if [ "$FAIL" -ne 0 ]; then
    echo "test-riscv64.sh: some comparisons failed" 1>&2
    exit 1
fi
