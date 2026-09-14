#!/bin/bash
# Whole-program integration test: o5a/o5l against a real, non-trivial
# C program's real lib_core/libc dependency closure (not hand-written
# .s fixtures) -- see docs/claude_notes/plan_hello_libc_linking.md for
# how this came about and the four real o5l bugs it found that no
# tests/linker/arm_diff/ fixture could ever have exposed (a real
# _main/rt0.s startup, a large linked .text, and real 5c's own
# multi-literal string-pooling convention).
#
# Unlike test-arm.sh & co (byte-for-byte against goken, one hand-
# written .s per construct), this checks that a real program actually
# runs correctly under qemu-arm: goken's own real 5a can't even
# assemble part of this closure (xix-only extensions like
# IndirectShift/CASE/BCASE have no real 5a grammar), so byte parity
# isn't the bar -- correct behavior is. See scripts/diff-c-program.sh
# and scripts/build-c-program.py for the actual pipeline.
#
# Usage: ./test-hello-libc.sh

set -e

cd "$(dirname "$0")"

GOKEN_ROOT=${GOKEN_ROOT:-$HOME/goken}
if [ ! -d "$GOKEN_ROOT" ]; then
    echo "test-hello-libc.sh: GOKEN_ROOT not found ($GOKEN_ROOT); skipping" 1>&2
    exit 0
fi

EXPECTED="hello from libc.a: 2 + 2 = 4"

echo "### tests/c/hello_libc/hello.c (real lib_core/libc closure)"
OUT=$(./scripts/diff-c-program.sh 5 tests/c/hello_libc/hello.c _main 2>&1) || {
    echo "$OUT"
    echo "test-hello-libc.sh: pipeline itself failed" 1>&2
    exit 1
}
echo "$OUT"

if echo "$OUT" | grep -qF -e "-- xix -- (exit 0): '$EXPECTED"; then
    echo
    echo "PASS: xix-built binary printed the expected output and exited 0"
else
    echo
    echo "test-hello-libc.sh: FAIL -- expected xix's binary to print" 1>&2
    echo "  '$EXPECTED...' and exit 0; see output above" 1>&2
    exit 1
fi
