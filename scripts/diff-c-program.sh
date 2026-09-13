#!/bin/bash
# Differential *program*-level stress test: compile a real C program
# (plus its real lib_core/libc dependency closure) with goken's own
# Nc/Na/Nl, and separately with xix's oNa/oNl, then compare the final
# ELF executables byte-for-byte and (when the right qemu-user is
# available) actually run both.
#
# Unlike scripts/diff-arm.sh & co (one hand-written .s fixture, one
# assemble+link call per side), this drives a whole pipeline:
#
#   1. enumerate the real lib_core/libc source file list for the given
#      arch/GOOS, the same way 'mk install' would build it (a
#      'mk -n -a ... install' dry-run) -- since o5l & friends have no
#      archive/-l support (see linker/CLI.ml), there is no way to link
#      against libc.a itself; instead every one of its source files is
#      assembled and linked in directly, alongside the target .c file.
#   2. for every .c file: compile it to Plan9 assembly text with
#      goken's real Nc -S (never xix's own, unfinished, occ/oNc) --
#      the same real text on both sides, so this is purely an
#      assembler+linker differential test, not a compiler one.
#   3. for every .s file (hand-written sources, e.g. arch/$cputype
#      startup/div/setjmp code): use as-is, no compilation step.
#   4. assemble every resulting .s file TWICE, once with goken's real
#      Na, once with xix's oNa -- independently, so a failure on one
#      side doesn't block the other. A file that fails on either side
#      is dropped from BOTH sides' link (a fair link needs the same
#      object set on both sides; see the summary table for what got
#      dropped and why -- that table *is* the point of this script,
#      it's the list of current real gaps).
#   5. link whatever assembled cleanly on both sides, once with
#      goken's real Nl, once with xix's oNl.
#   6. byte-compare the two final executables, and (qemu-user
#      permitting) run both and compare exit code + stdout.
#
# Usage:
#   ./scripts/diff-c-program.sh <arch> <main.c> [entry_symbol]
#
# <arch> is one of: 5 (arm), 6 (amd64), 7 (arm64), v (mips),
# i (riscv32), j (riscv64) -- see the ARCH_* table below. Only '5' is
# actually verified end-to-end so far (see
# docs/claude_notes/plan_hello_libc_linking.md); the others are wired
# the same way but each needs its own goken Nc's -S output checked for
# the same comma-padding print bug ARM's 5c had (fixed at the source,
# ~/goken/compilers/5c/list.c's Pconv) before they can work at all --
# see the "-S sanity check" step below, which fails loudly rather than
# silently producing garbage if that hasn't been done yet for a given
# arch.
#
# Env:
#   GOKEN_ROOT   path to a built goken checkout (default: ~/goken)

set -e

ARCH=$1
CFILE=$2
ENTRY=${3:-_main}

if [ -z "$ARCH" ] || [ -z "$CFILE" ]; then
    echo "usage: $0 <arch: 5|6|7|v|i|j> <main.c> [entry_symbol]" 1>&2
    exit 2
fi
if [ ! -f "$CFILE" ]; then
    echo "$0: no such file: $CFILE" 1>&2
    exit 2
fi

# claude: one row per arch -- goken tool-letter prefix, xix's
# corresponding o<X> prefix, the mkconfig objtype/cputype pair
# lib_core/libc's own mkfile expects, and the qemu-user binary that
# can run the resulting Linux/ELF binary. See scripts/diff-{arm,
# amd64,arm64,mips,riscv,riscv64}.sh for where each of these names
# was already established.
case "$ARCH" in
    5) GK=5; XX=o5; OBJTYPE_MK=arm;     QEMU=qemu-arm ;;
    6) GK=6; XX=o6; OBJTYPE_MK=amd64;   QEMU=qemu-x86_64 ;;
    7) GK=7; XX=o7; OBJTYPE_MK=arm64;   QEMU=qemu-aarch64 ;;
    v) GK=v; XX=ov; OBJTYPE_MK=mips;    QEMU=qemu-mips ;;
    i) GK=i; XX=oi; OBJTYPE_MK=riscv;   QEMU=qemu-riscv32 ;;
    j) GK=j; XX=oj; OBJTYPE_MK=riscv64; QEMU=qemu-riscv64 ;;
    *) echo "$0: unknown arch '$ARCH' (expected 5|6|7|v|i|j)" 1>&2; exit 2 ;;
esac

GOKEN_ROOT=${GOKEN_ROOT:-$HOME/goken}
if [ ! -d "$GOKEN_ROOT" ]; then
    echo "$0: GOKEN_ROOT not found ($GOKEN_ROOT); skipping" 1>&2
    exit 0
fi
# shellcheck disable=SC1090
BOOT_OBJTYPE=$(sed -n 's/^objtype=//p' "$GOKEN_ROOT/mkconfig")
GOKEN_BIN="$GOKEN_ROOT/ROOT/arch/$BOOT_OBJTYPE/bin"
GOKEN_NC="$GOKEN_BIN/${GK}c"
GOKEN_NA="$GOKEN_BIN/${GK}a"
GOKEN_NL="$GOKEN_BIN/${GK}l"
for t in "$GOKEN_NC" "$GOKEN_NA" "$GOKEN_NL"; do
    if [ ! -x "$t" ]; then
        echo "$0: goken $t not found (build goken first); skipping" 1>&2
        exit 0
    fi
done

XIX_ROOT=$(cd "$(dirname "$0")/.." && pwd)
XIX_NA="$XIX_ROOT/_build/default/bin_dune/${XX}a"
XIX_NL="$XIX_ROOT/_build/default/bin_dune/${XX}l"
if [ ! -x "$XIX_NA" ]; then XIX_NA=$(command -v "${XX}a" || true); fi
if [ ! -x "$XIX_NL" ]; then XIX_NL=$(command -v "${XX}l" || true); fi
if [ ! -x "$XIX_NA" ] || [ ! -x "$XIX_NL" ]; then
    echo "$0: xix ${XX}a/${XX}l not found (run 'dune build' first)" 1>&2
    exit 2
fi

LIBC_ROOT="$GOKEN_ROOT/lib_core/libc"
# claude: real CFLAGS a 'mk objtype=$OBJTYPE_MK' build of lib_core/libc
# (or tests/c/hello_libc) would use -- see mkfiles/mkfile.proto's
# CFLAGS_COMMON and mkfiles/$OBJTYPE_MK/mkfile's own -I addition (e.g.
# mkfiles/arm/mkfile: "-I$TOP/include/arch/arm", needed for u.h, which
# is per-arch, not in include/ or include/ALL/).
CFLAGS_LIBC=(-I"$GOKEN_ROOT/include" -I"$GOKEN_ROOT/include/ALL" \
             -I"$GOKEN_ROOT/include/arch/$OBJTYPE_MK")

TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT

echo "== enumerating lib_core/libc sources for objtype=$OBJTYPE_MK =="
( cd "$GOKEN_ROOT" && source env.sh > /dev/null 2>&1
  cd "$LIBC_ROOT" &&
  mk -n -a "objtype=$OBJTYPE_MK" "cputype=$OBJTYPE_MK" 'GOOS=linux' install
) > "$TMP/libc_dryrun.log" 2>&1 || true
grep -oE '[A-Za-z0-9_./]+\.(c|s)\b' "$TMP/libc_dryrun.log" | sort -u > "$TMP/libc_files.txt"
N_LIBC=$(wc -l < "$TMP/libc_files.txt")
echo "$N_LIBC lib_core/libc source files found"

# claude: compile in place (real path under $LIBC_ROOT, real cwd) so
# any file-local `#include "foo.h"` (quote-style, resolved relative to
# the including file's own directory) still finds its header -- only
# the *output* .s name is mangled (path separators -> '_') so files
# that share a basename across subdirectories can't collide, and so
# <>-scoped local symbols (see arm_port.md's "why not one concatenated
# file") stay one-object-per-source-file as required.
mangle() { echo "$1" | tr '/' '_'; }

CFILE_ABS=$(cd "$(dirname "$CFILE")" && pwd)/$(basename "$CFILE")
CBASE=$(mangle "main_$(basename "$CFILE" .c)")

ALL_UNITS=("$CBASE")
declare -A UNIT_C_PATH UNIT_S_PATH
UNIT_C_PATH[$CBASE]="$CFILE_ABS"
while read -r f; do
    [ -z "$f" ] && continue
    m=$(mangle "$f")
    case "$f" in
        *.c) UNIT_C_PATH[$m]="$LIBC_ROOT/$f" ;;
        *.s) UNIT_S_PATH[$m]="$LIBC_ROOT/$f" ;;
    esac
    ALL_UNITS+=("$m")
done < "$TMP/libc_files.txt"

echo "== compiling every .c to Plan9 assembly with goken's real ${GK}c -S =="
mkdir -p "$TMP/asm" "$TMP/obj5"
for u in "${ALL_UNITS[@]}"; do
    if [ -n "${UNIT_S_PATH[$u]:-}" ]; then
        cp "${UNIT_S_PATH[$u]}" "$TMP/asm/$u.s"
    else
        if ! "$GOKEN_NC" -S -D"$OBJTYPE_MK" "${CFLAGS_LIBC[@]}" \
             -o "$TMP/obj5/$u.$GK" "${UNIT_C_PATH[$u]}" \
             > "$TMP/asm/$u.s" 2> "$TMP/asm/$u.cerr"; then
            echo "  SKIP (${GK}c failed): $u -- $(head -1 "$TMP/asm/$u.cerr")"
            rm -f "$TMP/asm/$u.s"
        fi
    fi
done

echo "== sanity check: is ${GK}c -S output actually valid ${GK}a input? =="
# claude: catches the comma-padding print bug (see
# plan_hello_libc_linking.md / the ARM Pconv fix) up front, with a
# clear message, instead of every single file failing to assemble
# below for the same one reason.
SAMPLE=$(ls "$TMP"/asm/*.s 2>/dev/null | head -1)
if [ -n "$SAMPLE" ] && grep -qE '(^|[[:space:]]),([[:space:]]*$)|[[:space:]],[[:space:]]*$' "$SAMPLE"; then
    echo "$0: ${GK}c -S output for arch '$ARCH' still has the dangling-comma" 1>&2
    echo "print bug (see ARM's fix in ~/goken/compilers/5c/list.c's Pconv --" 1>&2
    echo "apply the same fix to compilers/${GK}c/list.c's Pconv before this" 1>&2
    echo "arch can be stress-tested this way). Aborting." 1>&2
    exit 1
fi

echo "== assembling every unit with both assemblers (failures dropped from both sides) =="
mkdir -p "$TMP/obj"
GOOD_UNITS=()
for u in "${ALL_UNITS[@]}"; do
    [ -f "$TMP/asm/$u.s" ] || continue
    ok=1
    if ! "$GOKEN_NA" -o "$TMP/obj/$u.goken.$GK" "$TMP/asm/$u.s" > "$TMP/obj/$u.goken.err" 2>&1; then
        echo "  DROP (goken ${GK}a failed): $u"
        ok=0
    fi
    if ! "$XIX_NA" -o "$TMP/obj/$u.xix.o$GK" "$TMP/asm/$u.s" > "$TMP/obj/$u.xix.err" 2>&1; then
        echo "  DROP (xix ${XX}a failed): $u  -- $(head -1 "$TMP/obj/$u.xix.err")"
        ok=0
    fi
    [ "$ok" = 1 ] && GOOD_UNITS+=("$u")
done
echo "${#GOOD_UNITS[@]} / ${#ALL_UNITS[@]} units assembled cleanly on both sides"

echo "== linking =="
GOKEN_OBJS=()
XIX_OBJS=()
for u in "${GOOD_UNITS[@]}"; do
    GOKEN_OBJS+=("$TMP/obj/$u.goken.$GK")
    XIX_OBJS+=("$TMP/obj/$u.xix.o$GK")
done

set +e
"$GOKEN_NL" -H7 -E "$ENTRY" -s -o "$TMP/goken.out" "${GOKEN_OBJS[@]}" 2> "$TMP/goken.link.err"
GOKEN_LINK_RC=$?
"$XIX_NL" -E "$ENTRY" -o "$TMP/xix.out" "${XIX_OBJS[@]}" 2> "$TMP/xix.link.err"
XIX_LINK_RC=$?
set -e

if [ "$GOKEN_LINK_RC" != 0 ]; then
    echo "goken ${GK}l FAILED (exit $GOKEN_LINK_RC):"; tail -20 "$TMP/goken.link.err"
fi
if [ "$XIX_LINK_RC" != 0 ]; then
    echo "xix ${XX}l FAILED (exit $XIX_LINK_RC):"; tail -20 "$TMP/xix.link.err"
fi
if [ "$GOKEN_LINK_RC" != 0 ] || [ "$XIX_LINK_RC" != 0 ]; then
    exit 1
fi

chmod +x "$TMP/goken.out" "$TMP/xix.out"

echo "== byte comparison =="
GOKEN_SIZE=$(wc -c < "$TMP/goken.out")
XIX_SIZE=$(wc -c < "$TMP/xix.out")
echo "goken: $GOKEN_SIZE bytes    xix: $XIX_SIZE bytes"
if cmp -s "$TMP/goken.out" "$TMP/xix.out"; then
    echo "PASS: byte-identical"
else
    echo "FAIL: byte differences (offset decimal, goken-byte xix-byte, octal):"
    cmp -l "$TMP/goken.out" "$TMP/xix.out" 2>&1 | head -20
fi

if command -v "$QEMU" >/dev/null 2>&1; then
    echo "== running under $QEMU =="
    set +e
    GOKEN_STDOUT=$("$QEMU" "$TMP/goken.out"); GOKEN_RC=$?
    XIX_STDOUT=$("$QEMU" "$TMP/xix.out"); XIX_RC=$?
    set -e
    echo "-- goken -- (exit $GOKEN_RC): $GOKEN_STDOUT"
    echo "-- xix   -- (exit $XIX_RC): $XIX_STDOUT"
    if [ "$GOKEN_RC" = "$XIX_RC" ] && [ "$GOKEN_STDOUT" = "$XIX_STDOUT" ]; then
        echo "PASS: same exit code and stdout"
    else
        echo "FAIL: exit code and/or stdout differ"
    fi
else
    echo "== $QEMU not found, skipping functional run ==" 1>&2
fi

echo "== summary =="
echo "units total=${#ALL_UNITS[@]} assembled-both-sides=${#GOOD_UNITS[@]} dropped=$((${#ALL_UNITS[@]} - ${#GOOD_UNITS[@]}))"
