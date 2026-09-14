#!/bin/bash
# Differential test driver for the amd64 port (complete -- see
# docs/claude_notes/amd64_port.md). Mirrors test-arm64.sh's shape.
#
# Usage: ./test-amd64.sh

set -e

cd "$(dirname "$0")"

# file[:extra_flags] pairs, same convention as test-arm.sh/test-arm64.sh.
CASES=(
    "amd64_diff/hello_linux.s:_start"
    "amd64_diff/cmp_jcc.s:_start"
    "amd64_diff/r8_r15.s:_start"
    "amd64_diff/movl_arith.s:_start"
    "amd64_diff/movw_arith.s:_start"
    "amd64_diff/movb_arith.s:_start"
    "amd64_diff/float_sd.s:_start"
    "amd64_diff/float_ss.s:_start"
    "amd64_diff/andorshift.s:_start"
    "amd64_diff/imm32_arith.s:_start"
    "amd64_diff/extend.s:_start"
    "amd64_diff/imm_yi32.s:_start"
    "amd64_diff/unary.s:_start"
    "amd64_diff/muldiv.s:_start"
    "amd64_diff/test_instr.s:_start"
    "amd64_diff/float_ext.s:_start"
    "amd64_diff/cmpxchg.s:_start"
    "amd64_diff/psllq.s:_start"
    "amd64_diff/indirect_call_jmp.s:_start"
    "amd64_diff/static_symbol.s:_start"
    "amd64_diff/imm64.s:_start"
)

FAIL=0
for c in "${CASES[@]}"; do
    IFS=':' read -r file entry <<< "$c"
    echo "### $file"
    if ! ../../scripts/diff-amd64.sh "$file" "$entry"; then
        FAIL=1
    fi
    echo
done

if [ "$FAIL" -ne 0 ]; then
    echo "test-amd64.sh: some comparisons failed" 1>&2
    exit 1
fi
