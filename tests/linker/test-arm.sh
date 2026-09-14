#!/bin/bash
# Differential test driver for the ARM port. Each fixture here is a
# single object file (no cross-object symbol resolution or archives),
# so what's mostly exercised is assembler codegen plus the linker's
# exec-format writing -- still filed under tests/linker/ since it's
# o5a+o5l's combined output being compared, and a sibling
# tests/linker/mips_diff/ is expected once MIPS gets the same
# treatment.
#
# Runs scripts/diff-arm.sh over the phase-0.5 baseline corpus (see
# docs/claude_notes/arm_port.md): .s files that only exercise
# constructs already ported in Codegen5.ml, byte-compared against
# goken's 5a/5l and (when qemu-arm is available) actually run.
#
# Usage: ./test-arm.sh
#
# See tests/linker/README.md for the fixture-naming/"_check" conventions.

set -e

cd "$(dirname "$0")"

# file[:extra_flags] pairs -- every fixture here uses TEXT _start, so
# the entry symbol itself isn't part of this list; extra_flags (e.g.
# -f for VFP) are optional and passed to both 5l and o5l identically.
CASES=(
    "arm_diff/hello_linux.s"
    "arm_diff/exit_linux.s"
    "arm_diff/addr.s"
    "arm_diff/call.s"
    "arm_diff/kitchen_sink.s"
    "arm_diff/bigimm_case12.s"
    "arm_diff/lcon_case13.s"
    "arm_diff/halfword_case70_71.s"
    "arm_diff/longoff_case30_31.s"
    "arm_diff/lacon_case4_12.s"
    "arm_diff/halflong_case72_73.s"
    "arm_diff/swp_case40.s"
    "arm_diff/fpa_case54.s"
    "arm_diff/vfp_case74_75.s:-f"
    "arm_diff/fixfloat_fpa_case55.s"
    "arm_diff/fixfloat_vfp_case76.s:-f"
    "arm_diff/float_mem_case50_51.s"
    "arm_diff/float_mem_vfp_case50_51.s:-f"
    "arm_diff/float_mem_longoff_case52_53.s"
    "arm_diff/float_mem_vfp_longoff_case52_53.s:-f"
    "arm_diff/fcr_move_case56_57.s"
    "arm_diff/mull_case17.s"
    "arm_diff/psr_move_case35_36_37.s"
    "arm_diff/movm_case38_39.s"
    "arm_diff/mcr_mrc.s"
    "arm_diff/static_symbol.s"
    "arm_diff/cret_leaf.s"
    "arm_diff/cret_framed.s"
    "arm_diff/movs_case.s"
    "arm_diff/shift_operand.s"
    "arm_diff/movf_const.s"
    "arm_diff/data_float.s"
    "arm_diff/data_addr_offset.s"
    "arm_diff/data_neg_int.s"
)

FAIL=0
for c in "${CASES[@]}"; do
    IFS=':' read -r file flags <<< "$c"
    echo "### $file${flags:+ (flags: $flags)}"
    if ! ../../scripts/diff-arm.sh "$file" "_start" "$flags"; then
        FAIL=1
    fi
    echo
done

if [ "$FAIL" -ne 0 ]; then
    echo "test-arm.sh: some comparisons failed" 1>&2
    exit 1
fi
