#!/bin/bash
# Whole-program integration test: oia/oil against a real, non-trivial
# C program's real lib_core/libc dependency closure (not hand-written
# .s fixtures) -- see docs/claude_notes/riscv_port.md for how this
# came about, mirroring hello_libc_arm/hello_libc_mips/hello_libc_arm64/
# hello_libc_amd64's own effort for RISC-V32.
#
# Found (and this port's own fixes closed) a real handful of gaps no
# tests/linker/riscv_diff/ fixture alone could have exposed:
#   - a 4th and 5th instance of this whole multi-arch effort's running
#     "goken's own -S output isn't valid re-assembleable input to
#     itself" bug family: a compiled JAL's own -S print omits its
#     REGLINK register (real ia's grammar has no bare register-less
#     JAL form at all -- fixed in goken's own compilers/ic/list.c),
#     and a "MOVW $0,off(R)" immediate-to-memory store that isn't a
#     real single instruction on any RISC-V hardware at all (real
#     goken's own optab.c only has "C_ZREG,C_SOREG -> OSTORE", i.e. a
#     REGISTER known to be zero, not a literal $0 -- worked around at
#     this port's own codegen level instead, since these particular
#     source files aren't reassembleable by goken's own ia either way)
#   - several previously entirely-unwired real mnemonics: bare "MOV"
#     (distinct from "MOVW"), "MOVWU"/"MOVUF"/"MOVUD", the BLE/BGT/
#     BLEU/BGTU pseudo-branch family (needing a real operand-swap-on-
#     linker-encode, not just a condition remap -- caught by hand-
#     decoding real bytes after an initial swap-free attempt produced
#     the wrong ones), float move/arithmetic/compare (FMOV/ArithF/
#     CmpF, including their own real-but-asymmetric rs1/rs2 mapping,
#     again only caught by hand-decoding real bytes), the RISC-V
#     M-extension (MUL/DIV/DIVU/REM/REMU), and a real, confirmed bug
#     in "MOV" vs "MOVW"'s own register-to-register encoding (the two
#     real mnemonics put the source register in a DIFFERENT operand
#     slot of the shared "ADD rd,rs,x0" idiom -- aliasing "MOV" onto
#     "MOVW"'s own token, as an earlier version of this port did after
#     confirming their *immediate* forms match, silently corrupted
#     real "MOV Rs,Rd" bytes once a real closure exercised it)
#   - a completely unimplemented Local/Param pseudo-frame addressing
#     story ("off(SP)"/"off(FP)", goken's own D_AUTO/D_PARAM,
#     including its own address-of form, "$sym+N(SP)") and float
#     memory access (FLD/FSD, both plain register-indirect and
#     SB-relative) -- both needed by nearly every function with real
#     local variables, not just this program's own leaf-level cases
#   - a genuine pre-existing bug in the RET rewrite for a leaf
#     function *with* a nonzero frame (linker/Rewritei.ml's own case
#     2): it updated `n.next` but never `n.instr` itself, leaving the
#     original un-transformed virtual RET node behind for codegen to
#     choke on -- affects any such function regardless of this
#     session's own Local/Param work, just first exposed by it
#   - a real forward-reference bug in the linker's own single-pass
#     symbol-table construction (linker/Layouti.ml): a TEXT symbol's
#     address isn't known until every earlier instruction has already
#     been sized, so an EARLIER file's "MOV $later_func(SB),R" (a real
#     forward reference -- fmt/fmtfd.c's own fmtfdinit takes the
#     address of fmt/fmtfdflush.c's own __fmtFdFlush, defined in a
#     *later* unit of the same link) used to crash outright; fixed by
#     deferring the actual value lookup into the binary-emission
#     thunk, which runs only after layout has fully completed
#   - the one that actually produced WRONG (not crashing) output at
#     runtime, not just at assemble/link time -- caught only by
#     running the real linked binary under qemu and checking its
#     actual printed text, not by any byte-identical differential
#     fixture (several of these source files aren't even assembleable
#     by goken's own real ia in the first place): "MOV $sym+N(SB),R"
#     (address-of-a-global-with-a-nonzero-offset) silently discarded
#     N entirely for every N<>0, always computing the address of
#     "sym+0" instead -- the offset field's own name in the AST match
#     arm was literally `_offsetTODO`. Broke real fmt/dofmt.c's own
#     "%d" digit-table setup ("MOV $.string<>+12(SB),R13", picking a
#     sub-table partway into a larger shared string-literal blob),
#     producing plausible-looking but wrong output ("hello from
#     libc.a: i + i = >" instead of "2 + 2 = 4") rather than a crash.
#
# Unlike test-riscv.sh (byte-for-byte against goken, one hand-written
# .s per construct), this checks that a real program actually runs
# correctly -- and, as the last bug above shows, is the ONLY check in
# this whole effort capable of catching some classes of real bug at
# all, since several of the exact source files that exposed it aren't
# even assembleable by goken's own real ia to begin with (no
# byte-identical reference exists for them), and a bug that discards
# an offset while still producing syntactically valid, differently-
# sized-or-not bytes doesn't necessarily show up as a byte-count or
# crash difference either.
#
# No goken checkout needed: closure.tgz (this directory) is a frozen
# snapshot of the real `ic -S` output for hello.c's real, minimal
# lib_core/libc dependency closure (36 files, found by
# scripts/find-c-closure.py's BFS -- see its own header), checked in
# as one compressed archive rather than 36 separate machine-generated
# .s files (keeps the diff to one binary blob, not thousands of lines
# of non-authored text). If GOKEN_ROOT does happen to point at a built
# goken checkout, scripts/build-c-program.py opportunistically also
# builds+compares against goken's own real ia/il -- purely
# informational (most of these files aren't assembleable by goken's
# own ia at all, for reasons predating and unrelated to this port --
# see this fixture's own docs/claude_notes/riscv_port.md entry),
# never required.
#
# To refresh the archive after a real lib_core/libc change (needs a
# built goken checkout) -- the archive wraps its files in a
# hello_libc_closure_il/ directory (il = goken's own RISC-V32 linker
# letter) rather than extracting flat, so a stray "tar xzf" elsewhere
# doesn't spray 36 files into the current directory:
#   rm -rf /tmp/hello_libc_closure_il
#   python3 ../../../scripts/find-c-closure.py i hello.c \
#       --out-dir /tmp/hello_libc_closure_il
#   tar czf closure.tgz -C /tmp hello_libc_closure_il
#
# Usage: ./test.sh (from this directory), or `make test` (see Makefile)

set -e

cd "$(dirname "$0")"
XIX_ROOT=$(cd ../../.. && pwd)

CLOSURE_SUBDIR="hello_libc_closure_il"
EXPECTED="hello from libc.a: 2 + 2 = 4"

TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT
tar xzf closure.tgz -C "$TMP"

echo "### tests/linker/hello_libc_riscv/closure.tgz (real lib_core/libc closure, frozen -- no goken needed)"
OUT=$(python3 "$XIX_ROOT/scripts/build-c-program.py" i "$TMP/$CLOSURE_SUBDIR" _main 2>&1) || {
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
