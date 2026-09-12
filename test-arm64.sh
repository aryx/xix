#!/bin/bash
# Differential test driver for the ARM64 port (first version -- see
# docs/claude_notes/notes_arm64_port_plan.txt). Mirrors test-arm.sh's
# shape and phase-0.5-baseline-corpus role.
#
# Usage: ./test-arm64.sh

set -e

cd "$(dirname "$0")"

# file[:extra_flags] pairs, same convention as test-arm.sh.
CASES=(
    "tests/linker/arm64_diff/exit_linux.s"
    "tests/linker/arm64_diff/kitchen_sink.s"
    "tests/linker/arm64_diff/call_leaf.s"
    "tests/linker/arm64_diff/call_leaf_with_frame.s"
    "tests/linker/arm64_diff/global_addr.s"
    "tests/linker/arm64_diff/w32_arith.s"
    "tests/linker/arm64_diff/sized_move.s"
)

FAIL=0
for c in "${CASES[@]}"; do
    IFS=':' read -r file flags <<< "$c"
    echo "### $file${flags:+ (flags: $flags)}"
    if ! ./scripts/diff-arm64.sh "$file" "_start" "$flags"; then
        FAIL=1
    fi
    echo
done

if [ "$FAIL" -ne 0 ]; then
    echo "test-arm64.sh: some comparisons failed" 1>&2
    exit 1
fi
