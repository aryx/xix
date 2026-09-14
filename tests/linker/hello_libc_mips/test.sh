#!/bin/bash
# Whole-program integration test: ova/ovl against a real, non-trivial
# C program's real lib_core/libc dependency closure (not hand-written
# .s fixtures) -- see docs/claude_notes/plan_hello_libc_linking.md for
# how this came about, mirroring hello_libc_arm/'s own effort for
# MIPS. Found (and this port's own fixes closed) a real assembler-side
# bug (goken's own vc -S output has the identical comma-padding Pconv
# artifact 5c/7c already needed fixing, plus a real backward-branch
# printing quirk -- see below), and several real o5a-class gaps no
# tests/linker/mips_diff/ fixture alone could have exposed: real
# hardware has no immediate-SUB/store-immediate/byte-halfword-
# indirect/etc instructions at all, needing goken's own real
# instruction-rewriting or this port's own xix-only REGTMP
# substitutes; a genuine encoding bug in this port's own first
# CMPEQ_/CMPGE_/CMPGT_ attempt (caught only by decoding real goken's
# linked bytes, not by reading the C source alone); and a real,
# confirmed "MOVW $sym+N(SB),Rt silently computes sym+0" bug (the
# exact same bug shape ARM32's own port found and fixed, just never
# ported to MIPS's own Codegenv.ml until this closure stress test
# found it too -- it broke hello.c's own real "%d" formatting until
# fixed).
#
# Unlike test-mips.sh (byte-for-byte against goken, one hand-written
# .s per construct), this checks that a real program actually runs
# correctly -- goken's own real vc -S output prints a negative "N(PC)"
# relative branch offset as its raw 32-bit-wraparound *unsigned*
# decimal value (e.g. "4294967291(PC)" for what's really -5(PC)), a
# real -S printing artifact this port's own shared
# assembler/Resolve_labels.ml now sign-extends at 32 bits before use
# (confirmed: real goken's own va reassembles it correctly too, via
# its own C int32 arithmetic wrapping the same way) -- so byte parity
# isn't the bar here either, correct behavior is.
#
# No goken checkout needed: closure.tgz (this directory) is a frozen
# snapshot of the real `vc -S` output for hello.c's real, minimal
# lib_core/libc dependency closure (36 files, found by
# scripts/find-c-closure.py's BFS -- see its own header), checked in
# as one compressed archive rather than 36 separate machine-generated
# .s files (keeps the diff to one binary blob, not thousands of lines
# of non-authored text). If GOKEN_ROOT does happen to point at a
# built goken checkout, scripts/build-c-program.py opportunistically
# also builds+compares against goken's own real va/vl -- purely
# informational, never required.
#
# To refresh the archive after a real lib_core/libc change (needs a
# built goken checkout) -- the archive wraps its files in a
# hello_libc_closure_vl/ directory (vl = goken's own MIPS linker
# letter) rather than extracting flat, so a stray "tar xzf" elsewhere
# doesn't spray 36 files into the current directory:
#   rm -rf /tmp/hello_libc_closure_vl
#   python3 ../../../scripts/find-c-closure.py v hello.c \
#       --out-dir /tmp/hello_libc_closure_vl
#   tar czf closure.tgz -C /tmp hello_libc_closure_vl
#
# Usage: ./test.sh (from this directory), or `make test` (see Makefile)

set -e

cd "$(dirname "$0")"
XIX_ROOT=$(cd ../../.. && pwd)

CLOSURE_SUBDIR="hello_libc_closure_vl"
EXPECTED="hello from libc.a: 2 + 2 = 4"

TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT
tar xzf closure.tgz -C "$TMP"

echo "### tests/linker/hello_libc_mips/closure.tgz (real lib_core/libc closure, frozen -- no goken needed)"
OUT=$(python3 "$XIX_ROOT/scripts/build-c-program.py" v "$TMP/$CLOSURE_SUBDIR" _main 2>&1) || {
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
