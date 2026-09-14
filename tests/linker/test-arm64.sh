#!/bin/bash
# Differential test driver for the ARM64 port (complete -- see
# docs/claude_notes/arm64_port.md). Mirrors test-arm.sh's shape.
#
# Usage: ./test-arm64.sh

set -e

cd "$(dirname "$0")"

# file[:extra_flags] pairs, same convention as test-arm.sh.
CASES=(
    "arm64_diff/exit_linux.s"
    "arm64_diff/kitchen_sink.s"
    "arm64_diff/call_leaf.s"
    "arm64_diff/call_leaf_with_frame.s"
    "arm64_diff/global_addr.s"
    "arm64_diff/w32_arith.s"
    "arm64_diff/sized_move.s"
    "arm64_diff/float_arith.s"
    "arm64_diff/float_mem.s"
    "arm64_diff/barrier.s"
    "arm64_diff/condsel_tbz.s"
    "arm64_diff/atomics.s"
    "arm64_diff/bitmask_logical.s"
    "arm64_diff/bitmask_w32.s"
    "arm64_diff/indirect_offset.s"
    "arm64_diff/arith_huge_imm.s"
    "arm64_diff/float_pool.s"
)

FAIL=0
for c in "${CASES[@]}"; do
    IFS=':' read -r file flags <<< "$c"
    echo "### $file${flags:+ (flags: $flags)}"
    if ! ../../scripts/diff-arm64.sh "$file" "_start" "$flags"; then
        FAIL=1
    fi
    echo
done

if [ "$FAIL" -ne 0 ]; then
    echo "test-arm64.sh: some comparisons failed" 1>&2
    exit 1
fi
