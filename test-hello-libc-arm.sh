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
# isn't the bar -- correct behavior is.
#
# No goken checkout needed: tests/linker/hello_libc_arm/closure.tgz is a
# frozen snapshot of the real `5c -S` output for hello.c's real,
# minimal lib_core/libc dependency closure (36 files, found by
# scripts/find-c-closure.py's BFS -- see its own header), checked in
# as one compressed archive rather than 36 separate machine-generated
# .s files (keeps the diff to one binary blob, not 7800+ lines of
# non-authored text). If GOKEN_ROOT does happen to point at a built
# goken checkout, scripts/build-c-program.py opportunistically also
# builds+compares against goken's own real N a/N l -- purely
# informational, never required.
#
# To refresh the archive after a real lib_core/libc change (needs a
# built goken checkout) -- the archive wraps its files in a
# hello_libc_closure_5l/ directory (5l = goken's own ARM linker letter)
# rather than extracting flat, so a stray "tar xzf" elsewhere doesn't
# spray 36 files into the current directory:
#   rm -rf /tmp/hello_libc_closure_5l
#   python3 scripts/find-c-closure.py 5 tests/linker/hello_libc_arm/hello.c \
#       --out-dir /tmp/hello_libc_closure_5l
#   tar czf tests/linker/hello_libc_arm/closure.tgz -C /tmp hello_libc_closure_5l
#
# Usage: ./test-hello-libc-arm.sh

set -e

cd "$(dirname "$0")"

FIXTURE_DIR="tests/linker/hello_libc_arm"
CLOSURE_SUBDIR="hello_libc_closure_5l"
EXPECTED="hello from libc.a: 2 + 2 = 4"

TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT
tar xzf "$FIXTURE_DIR/closure.tgz" -C "$TMP"

echo "### $FIXTURE_DIR/closure.tgz (real lib_core/libc closure, frozen -- no goken needed)"
OUT=$(python3 scripts/build-c-program.py 5 "$TMP/$CLOSURE_SUBDIR" _main 2>&1) || {
    echo "$OUT"
    echo "test-hello-libc-arm.sh: pipeline itself failed" 1>&2
    exit 1
}
echo "$OUT"

if echo "$OUT" | grep -qF -e "-- xix -- (exit 0): '$EXPECTED"; then
    echo
    echo "PASS: xix-built binary printed the expected output and exited 0"
else
    echo
    echo "test-hello-libc-arm.sh: FAIL -- expected xix's binary to print" 1>&2
    echo "  '$EXPECTED...' and exit 0; see output above" 1>&2
    exit 1
fi
