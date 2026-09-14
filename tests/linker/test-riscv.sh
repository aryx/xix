#!/bin/bash
# Differential test driver for the RISC-V (RV32) port (complete --
# see docs/claude_notes/riscv_port.md). Mirrors test-arm.sh/
# test-mips.sh.
#
# Usage: ./test-riscv.sh
#
# See tests/linker/README.md for the fixture-naming/"_check" conventions.

set -e

cd "$(dirname "$0")"

# Every fixture here uses TEXT _start, so there's no entry_symbol
# field -- just a bare list of .s files.
CASES=(
    "riscv_diff/hello_linux.s"
    "riscv_diff/arith_shift_case0_1.s"
    "riscv_diff/branch_case3.s"
    "riscv_diff/jal_case4.s"
    "riscv_diff/word_pseudo_case25_26.s"
    "riscv_diff/lui_case8.s"
    "riscv_diff/arith_imm_case2.s"
    "riscv_diff/mem_move_case6_7.s"
    "riscv_diff/system_csr_case22.s"
    "riscv_diff/jalr_case5.s"
    "riscv_diff/sb_value_case6_7.s"
    "riscv_diff/sb_value_case12_13.s"
    "riscv_diff/lcon_case14.s"
    "riscv_diff/lreg_case15_16.s"
    "riscv_diff/fcvt_case17.s"
    "riscv_diff/float_arith_case17.s"
    "riscv_diff/float_mem_case17b.s"
    "riscv_diff/sp_fp_pseudo.s"
    "riscv_diff/branch_pseudo_ble_bgt.s"
    "riscv_diff/muldiv_ext.s"
    "riscv_diff/addr_global_offset.s"
    "riscv_diff/fwd_text_ref.s"
    "riscv_diff/addr_reg_indirect.s"
)

FAIL=0
for file in "${CASES[@]}"; do
    echo "### $file"
    if ! ../../scripts/diff-riscv.sh "$file" "_start"; then
        FAIL=1
    fi
    echo
done

if [ "$FAIL" -ne 0 ]; then
    echo "test-riscv.sh: some comparisons failed" 1>&2
    exit 1
fi
