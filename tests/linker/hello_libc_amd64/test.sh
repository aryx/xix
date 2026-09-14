#!/bin/bash
# Whole-program integration test: o6a/o6l against a real, non-trivial
# C program's real lib_core/libc dependency closure (not hand-written
# .s fixtures) -- see docs/claude_notes/plan_hello_libc_linking.md for
# how this came about, mirroring hello_libc_arm/hello_libc_mips's own
# effort for amd64. Found (and this port's own fixes closed) a real
# handful of gaps no tests/linker/amd64_diff/ fixture alone could have
# exposed: real x86 SIB scaled-index addressing ("(BX)(CX*4)",
# "tab<>+0(SB)(CX*8)"), BP/R13 as an ordinary memory base (mod=00
# meaning RIP-relative, not "no displacement", the same real x86 quirk
# every arch's own memory-operand story eventually hits), the reverse-
# direction CMP row ("CMPQ Rs,mem"), a literal-float-source auto-
# generated DATA symbol (goken's own linkers/6l/obj.c D_FCONST
# preprocessing, "MOVSD $1.0,X0"), real amd64 JMP/Jcc short-vs-near
# branch relaxation, and -- the one that actually crashed a real
# program at runtime, not just at assemble/link time -- a missing
# automatic prologue/epilogue: real 6a synthesizes a "SUB
# $autosize,SP"/"ADD $autosize,SP" around a TEXT's own body based on
# its declared frame size, and (a second, genuinely distinct bug, only
# found once the first was fixed and the program actually ran) a
# *named* local variable reference against SP (e.g. "f+-104(SP)") uses
# goken's own "pseudo-SP" convention (hardware offset = autosize + N,
# not the raw N) -- this port's earlier version used the raw offset
# directly, which happened to produce syntactically valid bytes for
# every smaller fixture tested so far, silently corrupting the stack
# (and crashing at a completely unrelated later point) only once a
# real closure exercised a function whose true hardware offset
# differed enough from its source-level offset to matter.
#
# Unlike test-amd64.sh (byte-for-byte against goken, one hand-written
# .s per construct), this checks that a real program actually runs
# correctly -- goken's own real 6l does dead-code elimination this
# port doesn't (a benign "RET;RET" double-return dead-code pattern
# common in real 6c output keeps its own second, unreachable epilogue+
# RET here, harmless but a real byte-count difference), so byte parity
# isn't the bar for the *final linked binary* either, correct
# behavior is (test-amd64.sh's own per-construct fixtures still check
# byte parity where it's meaningful).
#
# No goken checkout needed: closure.tgz (this directory) is a frozen
# snapshot of the real `6c -S` output for hello.c's real, minimal
# lib_core/libc dependency closure (35 files, found by
# scripts/find-c-closure.py's BFS -- see its own header), checked in
# as one compressed archive rather than 35 separate machine-generated
# .s files (keeps the diff to one binary blob, not thousands of lines
# of non-authored text). If GOKEN_ROOT does happen to point at a
# built goken checkout, scripts/build-c-program.py opportunistically
# also builds+compares against goken's own real 6a/6l -- purely
# informational, never required.
#
# To refresh the archive after a real lib_core/libc change (needs a
# built goken checkout) -- the archive wraps its files in a
# hello_libc_closure_6l/ directory (6l = goken's own amd64 linker
# letter) rather than extracting flat, so a stray "tar xzf" elsewhere
# doesn't spray 35 files into the current directory:
#   rm -rf /tmp/hello_libc_closure_6l
#   python3 ../../../scripts/find-c-closure.py 6 hello.c \
#       --out-dir /tmp/hello_libc_closure_6l
#   tar czf closure.tgz -C /tmp hello_libc_closure_6l
#
# Usage: ./test.sh (from this directory), or `make test` (see Makefile)

set -e

cd "$(dirname "$0")"
XIX_ROOT=$(cd ../../.. && pwd)

CLOSURE_SUBDIR="hello_libc_closure_6l"
EXPECTED="hello from libc.a: 2 + 2 = 4"

TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT
tar xzf closure.tgz -C "$TMP"

echo "### tests/linker/hello_libc_amd64/closure.tgz (real lib_core/libc closure, frozen -- no goken needed)"
OUT=$(python3 "$XIX_ROOT/scripts/build-c-program.py" 6 "$TMP/$CLOSURE_SUBDIR" _main 2>&1) || {
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
