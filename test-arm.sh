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
# docs/claude_notes/notes_arm_port_plan.txt and
# docs/claude_notes/todo_arm_port.org): .s files that only exercise
# constructs already ported in Codegen5.ml, byte-compared against
# goken's 5a/5l and (when qemu-arm is available) actually run.
#
# Usage: ./test-arm.sh

set -e

cd "$(dirname "$0")"

# file:entry_symbol[:extra_flags] triples -- entry defaults to _main
# (the linkers' default) when not TEXT _start; extra_flags (e.g. -f
# for VFP) are optional and passed to both 5l and o5l identically.
CASES=(
    "tests/linker/arm_diff/hello_linux_arm.s:_start"
    "tests/linker/arm_diff/exit_linux_arm.s:_start"
    "tests/linker/arm_diff/addr_arm.s:_start"
    "tests/linker/arm_diff/call_arm.s:_start"
    "tests/linker/arm_diff/kitchen_sink.s:_start"
    "tests/linker/arm_diff/bigimm_arm.s:_start"
    "tests/linker/arm_diff/lcon_arm.s:_start"
    "tests/linker/arm_diff/halfword_arm.s:_start"
    "tests/linker/arm_diff/longoff_arm.s:_start"
    "tests/linker/arm_diff/lacon_arm.s:_start"
    "tests/linker/arm_diff/halflong_arm.s:_start"
    "tests/linker/arm_diff/swp_arm.s:_start"
    "tests/linker/arm_diff/fpa_arm.s:_start"
    "tests/linker/arm_diff/vfp_arm.s:_start:-f"
)

FAIL=0
for c in "${CASES[@]}"; do
    IFS=':' read -r file entry flags <<< "$c"
    echo "### $file (entry $entry)${flags:+ (flags: $flags)}"
    if ! ./scripts/diff-arm.sh "$file" "$entry" "$flags"; then
        FAIL=1
    fi
    echo
done

if [ "$FAIL" -ne 0 ]; then
    echo "test-arm.sh: some comparisons failed" 1>&2
    exit 1
fi
