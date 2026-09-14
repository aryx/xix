#!/bin/bash
# Whole-program integration test: o7a/o7l against a real, non-trivial
# C program's real lib_core/libc dependency closure (not hand-written
# .s fixtures) -- see docs/claude_notes/plan_hello_libc_linking.md for
# how this came about, mirroring hello_libc_arm/'s own effort for
# ARM64. Found (and this port's own fixes closed) several real gaps
# no tests/linker/arm64_diff/ fixture alone could have exposed: a
# real rt0.s startup, real lib_core/libc's own use of ANDW/ORRW/EORW
# bitmask immediates, register-relative Indirect addresses beyond the
# scaled-12-bit fast path, ADD/SUB/CMP immediates too big for addcon,
# a float literal-pool WORD-splicing bug, and -- the one that took
# actual native execution (not just linking) to catch -- a real
# "(FP)"/"(SP)" offset formula bug silently wrong for any non-trivial
# frame (see arm64_diff/fp_offset.s/sp_offset.s and Codegen7.ml's own
# local_param_offset comment).
#
# Unlike test-arm64.sh (byte-for-byte against goken, one hand-written
# .s per construct), this checks that a real program actually runs
# correctly: goken's own real 7a can't even assemble part of this
# closure (its own real 7c -S output uses raw "R31" register syntax
# its own 7a lexer doesn't accept -- a genuine goken self-consistency
# bug, confirmed directly, not a xix guess), so byte parity isn't the
# bar here -- correct behavior is. This host is itself aarch64, so
# the binary can also just be run directly (no qemu needed) -- see
# the Makefile's own "run" target for that shortcut; this script
# still goes through qemu-aarch64 (via build-c-program.py) so the
# test works the same way on any host.
#
# No goken checkout needed: closure.tgz (this directory) is a frozen
# snapshot of the real `7c -S` output for hello.c's real, minimal
# lib_core/libc dependency closure (35 files, found by
# scripts/find-c-closure.py's BFS -- see its own header), checked in
# as one compressed archive rather than 35 separate machine-generated
# .s files (keeps the diff to one binary blob, not thousands of lines
# of non-authored text). If GOKEN_ROOT does happen to point at a
# built goken checkout, scripts/build-c-program.py opportunistically
# also tries building+comparing against goken's own real 7a/7l --
# purely informational, never required (and, per above, goken's own
# 7a will genuinely fail on part of this closure).
#
# To refresh the archive after a real lib_core/libc change (needs a
# built goken checkout) -- the archive wraps its files in a
# hello_libc_closure_7l/ directory (7l = goken's own ARM64 linker
# letter) rather than extracting flat, so a stray "tar xzf" elsewhere
# doesn't spray 35 files into the current directory:
#   rm -rf /tmp/hello_libc_closure_7l
#   python3 ../../../scripts/find-c-closure.py 7 hello.c \
#       --out-dir /tmp/hello_libc_closure_7l
#   tar czf closure.tgz -C /tmp hello_libc_closure_7l
#
# Usage: ./test.sh (from this directory), or `make test` (see Makefile)

set -e

cd "$(dirname "$0")"
XIX_ROOT=$(cd ../../.. && pwd)

CLOSURE_SUBDIR="hello_libc_closure_7l"
EXPECTED="hello from libc.a: 2 + 2 = 4"

TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT
tar xzf closure.tgz -C "$TMP"

echo "### tests/linker/hello_libc_arm64/closure.tgz (real lib_core/libc closure, frozen -- no goken needed)"
OUT=$(python3 "$XIX_ROOT/scripts/build-c-program.py" 7 "$TMP/$CLOSURE_SUBDIR" _main 2>&1) || {
    echo "$OUT"
    echo "test.sh: pipeline itself failed" 1>&2
    exit 1
}
echo "$OUT"

if echo "$OUT" | grep -qF -e "-- xix -- (exit 0): '$EXPECTED"; then
    echo
    echo "PASS: xix-built binary printed the expected output and exited 0"
else
    echo
    echo "test.sh: FAIL -- expected xix's binary to print" 1>&2
    echo "  '$EXPECTED...' and exit 0; see output above" 1>&2
    exit 1
fi
