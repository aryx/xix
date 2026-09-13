#!/bin/bash
# Differential test driver for the amd64 port (in progress -- see
# docs/claude_notes/plan_amd64_port.md). Mirrors test-arm64.sh's shape.
#
# Usage: ./test-amd64.sh

set -e

cd "$(dirname "$0")"

# file[:extra_flags] pairs, same convention as test-arm.sh/test-arm64.sh.
CASES=(
    "tests/linker/amd64_diff/hello_linux.s:_start"
    "tests/linker/amd64_diff/cmp_jcc.s:_start"
    "tests/linker/amd64_diff/r8_r15.s:_start"
    "tests/linker/amd64_diff/movl_arith.s:_start"
    "tests/linker/amd64_diff/movw_arith.s:_start"
    "tests/linker/amd64_diff/movb_arith.s:_start"
    "tests/linker/amd64_diff/float_sd.s:_start"
    "tests/linker/amd64_diff/float_ss.s:_start"
    "tests/linker/amd64_diff/indirect_call_jmp.s:_start"
    "tests/linker/amd64_diff/static_symbol.s:_start"
    "tests/linker/amd64_diff/imm64.s:_start"
)

FAIL=0
for c in "${CASES[@]}"; do
    IFS=':' read -r file entry <<< "$c"
    echo "### $file"
    if ! ./scripts/diff-amd64.sh "$file" "$entry"; then
        FAIL=1
    fi
    echo
done

if [ "$FAIL" -ne 0 ]; then
    echo "test-amd64.sh: some comparisons failed" 1>&2
    exit 1
fi
