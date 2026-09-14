#!/bin/bash
# Whole-program integration test: oja/ojl against a real, non-trivial
# C program's real lib_core/libc dependency closure (not hand-written
# .s fixtures) -- see docs/claude_notes/riscv_port.md for how this
# came about, mirroring hello_libc_riscv/'s own effort for RISC-V32
# (read that one first -- same methodology, run in a later session,
# sharing nearly all of its Codegeni.ml/Rewritei.ml/Layouti.ml/
# Ast_asmi.ml infrastructure).
#
# Found (and this port's own fixes closed) a much smaller set of new
# gaps than RISC-V32 needed, since almost everything RISC-V64 shares
# with RISC-V32 (Local/Param addressing, float arithmetic/compare,
# the M-extension, the address-of-global offset bug, etc.) was
# already fixed by that earlier session -- what was genuinely new
# here is riscv64's own explicit-32-bit-view "*W" opcode family
# (real RISC-V's OOP_32/OOP_IMM_32 major opcodes, 0x3b/0x1b, instead
# of OOP/OOP_IMM's 0x33/0x13 -- ADDW/SUBW/SLLW/SRLW/SRAW/MULW/DIVW/
# DIVUW/REMW/REMUW, plus ADDIW's own immediate form), a V__ (bare,
# pointer-width) sibling for the "MOV R,sym(SB)"/"MOV sym(SB),R"
# store/load-to-global and "MOV $0,off(R)" zero-store cases (W__ was
# already wired, but riscv64's own real 64-bit pointers need the SD/
# LD-vs-SW/LW is_64 branch those cases didn't have yet), and one more
# instance of this whole effort's running "goken's own -S output
# isn't valid re-assembleable input to itself" bug family: a compiled
# Prog's own small, valid ADDIW immediate (e.g. -1) gets printed as
# its unsigned 32-bit representation (4294967295) instead, which real
# goken's own il then rejects outright as "illegal combination" (no
# large-constant fallback exists for this op at all) -- worked around
# at this port's own level by reinterpreting the raw immediate as a
# 32-bit signed quantity before the encodability check, since there's
# no goken reference to match bytes against either way.
#
# No goken checkout needed: closure.tgz (this directory) is a frozen
# snapshot of the real `jc -S` output for hello.c's real, minimal
# lib_core/libc dependency closure (35 files -- one fewer than
# RISC-V32's own 36, since riscv64's native 64-bit integers don't
# need port/vlrt.c's software int64 emulation helpers -- found by
# scripts/find-c-closure.py's BFS), checked in as one compressed
# archive rather than 35 separate machine-generated .s files (keeps
# the diff to one binary blob, not thousands of lines of non-authored
# text). If GOKEN_ROOT does happen to point at a built goken
# checkout, scripts/build-c-program.py opportunistically also
# builds+compares against goken's own real ja/jl -- purely
# informational (many of these files aren't assembleable by goken's
# own ja at all, for reasons predating and unrelated to this port --
# same files as RISC-V32's own, see that fixture's own note), never
# required.
#
# To refresh the archive after a real lib_core/libc change (needs a
# built goken checkout) -- the archive wraps its files in a
# hello_libc_closure_jl/ directory (jl = goken's own RISC-V64 linker
# letter) rather than extracting flat, so a stray "tar xzf" elsewhere
# doesn't spray 35 files into the current directory:
#   rm -rf /tmp/hello_libc_closure_jl
#   python3 ../../../scripts/find-c-closure.py j hello.c \
#       --out-dir /tmp/hello_libc_closure_jl
#   tar czf closure.tgz -C /tmp hello_libc_closure_jl
#
# Usage: ./test.sh (from this directory), or `make test` (see Makefile)

set -e

cd "$(dirname "$0")"
XIX_ROOT=$(cd ../../.. && pwd)

CLOSURE_SUBDIR="hello_libc_closure_jl"
EXPECTED="hello from libc.a: 2 + 2 = 4"

TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT
tar xzf closure.tgz -C "$TMP"

echo "### tests/linker/hello_libc_riscv64/closure.tgz (real lib_core/libc closure, frozen -- no goken needed)"
OUT=$(python3 "$XIX_ROOT/scripts/build-c-program.py" j "$TMP/$CLOSURE_SUBDIR" _main 2>&1) || {
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
