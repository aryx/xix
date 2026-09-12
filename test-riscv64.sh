#!/bin/bash
# Differential test driver for the RISC-V64 port (ojl, sharing
# Codegeni.ml/Rewritei.ml/Layouti.ml/Ast_asmi.ml with the RV32 port --
# goken's ja/jl are literally the same binaries as ia/il). Mirrors
# test-riscv.sh -- see docs/claude_notes/notes_riscv_port_plan.txt and
# docs/claude_notes/todo_riscv_port.org.
#
# Usage: ./test-riscv64.sh
#
# See tests/linker/README.md for the fixture-naming/"_check" conventions.

set -e

cd "$(dirname "$0")"

# Every fixture here uses TEXT _start, so there's no entry_symbol
# field -- just a bare list of .s files.
CASES=(
    "tests/linker/riscv64_diff/hello_linux.s"
    "tests/linker/riscv64_diff/arith_shift_case0_1.s"
    "tests/linker/riscv64_diff/branch_case3.s"
    "tests/linker/riscv64_diff/jal_case4.s"
    "tests/linker/riscv64_diff/word_pseudo_case25_26.s"
    "tests/linker/riscv64_diff/lui_case8.s"
    "tests/linker/riscv64_diff/arith_imm_case2.s"
    "tests/linker/riscv64_diff/mem_move_case6_7.s"
    "tests/linker/riscv64_diff/system_csr_case22.s"
    "tests/linker/riscv64_diff/jalr_case5.s"
    "tests/linker/riscv64_diff/sb_value_case6_7.s"
    "tests/linker/riscv64_diff/sb_value_case12_13.s"
    "tests/linker/riscv64_diff/lcon_case14.s"
)

FAIL=0
for file in "${CASES[@]}"; do
    echo "### $file"
    if ! ./scripts/diff-riscv64.sh "$file" "_start"; then
        FAIL=1
    fi
    echo
done

if [ "$FAIL" -ne 0 ]; then
    echo "test-riscv64.sh: some comparisons failed" 1>&2
    exit 1
fi
