#!/bin/bash
# Differential test: assemble+link a .s file with both goken's ja/jl
# (the C Plan 9 RISC-V64 reference) and xix's oja/ojl, then compare the
# final ELF executables byte-for-byte and (when qemu-riscv64 is
# available) run both under it.
#
# ja/jl are literally the same binaries as ia/il (goken dispatches
# thechar on argv0's basename at runtime, see
# mkfiles/riscv64/mkfile) -- mirrors scripts/diff-riscv.sh exactly,
# just pointed at the 'j' names and qemu-riscv64. See
# docs/claude_notes/riscv_port.md for why only the final
# executable is ever compared, never the intermediate object files.
#
# Usage:
#   ./scripts/diff-riscv64.sh tests/linker/riscv64_diff/hello_linux_riscv64.s [entry_symbol]
#
# Env:
#   GOKEN_ROOT   path to a built goken checkout (default: ~/goken)

set -e

SFILE=$1
ENTRY=${2:-_main}

if [ -z "$SFILE" ]; then
    echo "usage: $0 <file.s> [entry_symbol]" 1>&2
    exit 2
fi
if [ ! -f "$SFILE" ]; then
    echo "$0: no such file: $SFILE" 1>&2
    exit 2
fi

GOKEN_ROOT=${GOKEN_ROOT:-$HOME/goken}
if [ ! -d "$GOKEN_ROOT" ]; then
    echo "$0: GOKEN_ROOT not found ($GOKEN_ROOT); skipping" 1>&2
    exit 0
fi
# shellcheck disable=SC1090
OBJTYPE=$(sed -n 's/^objtype=//p' "$GOKEN_ROOT/mkconfig")
GOKEN_BIN="$GOKEN_ROOT/ROOT/arch/$OBJTYPE/bin"
GOKEN_JA="$GOKEN_BIN/ja"
GOKEN_JL="$GOKEN_BIN/jl"
if [ ! -x "$GOKEN_JA" ] || [ ! -x "$GOKEN_JL" ]; then
    echo "$0: goken ja/jl not found under $GOKEN_BIN (build goken first); skipping" 1>&2
    exit 0
fi

XIX_ROOT=$(cd "$(dirname "$0")/.." && pwd)
# Prefer this repo's freshly-built binaries over whatever oja/ojl
# might be on PATH -- we're porting/testing this checkout, not some
# other installed version.
XIX_OJA="$XIX_ROOT/_build/default/bin_dune/oja"
XIX_OJL="$XIX_ROOT/_build/default/bin_dune/ojl"
if [ ! -x "$XIX_OJA" ]; then XIX_OJA=$(command -v oja || true); fi
if [ ! -x "$XIX_OJL" ]; then XIX_OJL=$(command -v ojl || true); fi
if [ ! -x "$XIX_OJA" ] || [ ! -x "$XIX_OJL" ]; then
    echo "$0: xix oja/ojl not found (run 'dune build' first)" 1>&2
    exit 2
fi

TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT

BASE=$(basename "$SFILE" .s)
cp "$SFILE" "$TMP/$BASE.s"
cd "$TMP"

echo "== assembling+linking with goken ($GOKEN_JA / $GOKEN_JL) =="
"$GOKEN_JA" -o "$BASE.goken.j" "$BASE.s"
# -c: disable goken's instruction compression (RVC) -- xix's port
# never emits compressed instructions, so this is needed to compare
# apples to apples (see Codegeni.ml). -s: strip goken's native
# Plan9 symbol/debug table, same reasoning as the ARM/MIPS harnesses.
"$GOKEN_JL" -E "$ENTRY" -c -s -o "$BASE.goken.out" "$BASE.goken.j"

echo "== assembling+linking with xix ($XIX_OJA / $XIX_OJL) =="
"$XIX_OJA" -o "$BASE.xix.oj" "$BASE.s"
"$XIX_OJL" -E "$ENTRY" -o "$BASE.xix.out" "$BASE.xix.oj"

chmod +x "$BASE.goken.out" "$BASE.xix.out"

echo "== byte comparison =="
GOKEN_SIZE=$(wc -c < "$BASE.goken.out")
XIX_SIZE=$(wc -c < "$BASE.xix.out")
echo "goken: $GOKEN_SIZE bytes    xix: $XIX_SIZE bytes"
if cmp -s "$BASE.goken.out" "$BASE.xix.out"; then
    echo "PASS: byte-identical"
else
    echo "FAIL: byte differences (offset decimal, goken-byte xix-byte, octal):"
    cmp -l "$BASE.goken.out" "$BASE.xix.out" 2>&1 | head -20
fi

if command -v qemu-riscv64 >/dev/null 2>&1; then
    echo "== running under qemu-riscv64 =="
    # a test program's exit code is often intentionally non-zero --
    # don't let `set -e` treat that as a script failure.
    echo "-- goken --"
    set +e
    qemu-riscv64 "./$BASE.goken.out"; GOKEN_RC=$?
    set -e
    echo "(exit code: $GOKEN_RC)"
    echo "-- xix --"
    set +e
    qemu-riscv64 "./$BASE.xix.out"; XIX_RC=$?
    set -e
    echo "(exit code: $XIX_RC)"
    if [ "$GOKEN_RC" = "$XIX_RC" ]; then
        echo "PASS: same exit code"
    else
        echo "FAIL: exit codes differ (goken=$GOKEN_RC xix=$XIX_RC)"
    fi
else
    echo "== qemu-riscv64 not found, skipping functional run ==" 1>&2
fi
