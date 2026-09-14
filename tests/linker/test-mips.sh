#!/bin/bash
# Differential test driver for the MIPS port. Mirrors test-arm.sh --
# see docs/claude_notes/mips_port.md.
#
# Usage: ./test-mips.sh
#
# See tests/linker/README.md for the fixture-naming/"_check" conventions.

set -e

cd "$(dirname "$0")"

# Every fixture here uses TEXT _start, so there's no entry_symbol
# field -- just a bare list of .s files.
CASES=(
    "mips_diff/hello_linux.s"
    "mips_diff/exit_linux.s"
    "mips_diff/addr.s"
    "mips_diff/kitchen_sink.s"
    "mips_diff/arith_rrr_case2.s"
    "mips_diff/shift_reg_case9.s"
    "mips_diff/movw_andcon_case3.s"
    "mips_diff/movbh_case12_13.s"
    "mips_diff/shift_imm_case16.s"
    "mips_diff/mullohi_case20_21.s"
    "mips_diff/mul_case22.s"
    "mips_diff/movw_ucon_case24.s"
    "mips_diff/add_bigimm_case23_25.s"
    "mips_diff/lacon_case26.s"
    "mips_diff/float_arith_case32_33.s"
    "mips_diff/float_int_move_case30_31.s"
    "mips_diff/float_const_case34.s"
    "mips_diff/mem_move_case7_8.s"
    "mips_diff/float_move_case27_28.s"
    "mips_diff/cop0_move_case37_38.s"
    "mips_diff/word_case40.s"
    "mips_diff/fcr_move_case41_42.s"
    "mips_diff/rfe_case39.s"
    "mips_diff/atomic_case47_48.s"
)
# The corresponding *_check.s fixtures (functional-only: same
# qemu-mips behavior on both sides, but not byte-identical, because
# goken's sched.c hoists real instructions into delay slots instead
# of the plain NOP this port emits) are deliberately not in this
# list -- see tests/linker/README.md for the "_check" convention,
# and each such fixture's own header comment for its specific
# hazard. Run one manually with scripts/diff-mips.sh to see its
# (small, known, scheduler-only) diff.

FAIL=0
for file in "${CASES[@]}"; do
    echo "### $file"
    if ! ../../scripts/diff-mips.sh "$file" "_start"; then
        FAIL=1
    fi
    echo
done

if [ "$FAIL" -ne 0 ]; then
    echo "test-mips.sh: some comparisons failed" 1>&2
    exit 1
fi
