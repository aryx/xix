#!/bin/bash
# Differential test driver for the RISC-V (RV32) port. Mirrors
# test-arm.sh/test-mips.sh -- see
# docs/claude_notes/notes_riscv_port_plan.txt and
# docs/claude_notes/todo_riscv_port.org.
#
# Usage: ./test-riscv.sh
#
# See tests/linker/README.md for the fixture-naming/"_check" conventions.

set -e

cd "$(dirname "$0")"

# Every fixture here uses TEXT _start, so there's no entry_symbol
# field -- just a bare list of .s files.
CASES=(
    "tests/linker/riscv_diff/hello_linux.s"
    "tests/linker/riscv_diff/arith_shift_case0_1.s"
    "tests/linker/riscv_diff/branch_case3.s"
    "tests/linker/riscv_diff/jal_case4.s"
    "tests/linker/riscv_diff/word_pseudo_case25_26.s"
    "tests/linker/riscv_diff/lui_case8.s"
    "tests/linker/riscv_diff/arith_imm_case2.s"
    "tests/linker/riscv_diff/mem_move_case6_7.s"
    "tests/linker/riscv_diff/system_csr_case22.s"
    "tests/linker/riscv_diff/jalr_case5.s"
    "tests/linker/riscv_diff/sb_value_case6_7.s"
    "tests/linker/riscv_diff/sb_value_case12_13.s"
    "tests/linker/riscv_diff/lcon_case14.s"
    "tests/linker/riscv_diff/lreg_case15_16.s"
)

FAIL=0
for file in "${CASES[@]}"; do
    echo "### $file"
    if ! ./scripts/diff-riscv.sh "$file" "_start"; then
        FAIL=1
    fi
    echo
done

if [ "$FAIL" -ne 0 ]; then
    echo "test-riscv.sh: some comparisons failed" 1>&2
    exit 1
fi
