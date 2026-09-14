#!/bin/bash
# Differential test: assemble+link a .s file with both goken's ia/il
# (the C Plan 9 RISC-V reference) and xix's oia/oil, then compare the
# final ELF executables byte-for-byte and (when qemu-riscv32 is
# available) run both under it.
#
# Mirrors scripts/diff-arm.sh/diff-mips.sh -- see
# docs/claude_notes/arm_port.md for why only the final
# executable is ever compared, never the intermediate object files.
#
# Usage:
#   ./scripts/diff-riscv.sh tests/linker/riscv_diff/hello_linux_riscv.s [entry_symbol]
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
GOKEN_IA="$GOKEN_BIN/ia"
GOKEN_IL="$GOKEN_BIN/il"
if [ ! -x "$GOKEN_IA" ] || [ ! -x "$GOKEN_IL" ]; then
    echo "$0: goken ia/il not found under $GOKEN_BIN (build goken first); skipping" 1>&2
    exit 0
fi

XIX_ROOT=$(cd "$(dirname "$0")/.." && pwd)
# Prefer this repo's freshly-built binaries over whatever oia/oil
# might be on PATH -- we're porting/testing this checkout, not some
# other installed version.
# claude: uses the top-level bin_dune/ symlink (-> _build/install/
# default/bin/), not _build/default/bin_dune/ like this script used
# to -- found empirically that the latter can go stale after a plain
# `dune build` (it's only refreshed reliably by the install
# machinery), while bin_dune/ always tracks the freshest Main.exe.
XIX_OIA="$XIX_ROOT/bin_dune/oia"
XIX_OIL="$XIX_ROOT/bin_dune/oil"
if [ ! -x "$XIX_OIA" ]; then XIX_OIA=$(command -v oia || true); fi
if [ ! -x "$XIX_OIL" ]; then XIX_OIL=$(command -v oil || true); fi
if [ ! -x "$XIX_OIA" ] || [ ! -x "$XIX_OIL" ]; then
    echo "$0: xix oia/oil not found (run 'dune build' first)" 1>&2
    exit 2
fi

TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT

BASE=$(basename "$SFILE" .s)
cp "$SFILE" "$TMP/$BASE.s"
cd "$TMP"

echo "== assembling+linking with goken ($GOKEN_IA / $GOKEN_IL) =="
"$GOKEN_IA" -o "$BASE.goken.i" "$BASE.s"
# -c: disable goken's instruction compression (RVC) -- xix's port
# never emits compressed instructions, so this is needed to compare
# apples to apples (see Codegeni.ml). -s: strip goken's native
# Plan9 symbol/debug table, same reasoning as the ARM/MIPS harnesses.
"$GOKEN_IL" -E "$ENTRY" -c -s -o "$BASE.goken.out" "$BASE.goken.i"

echo "== assembling+linking with xix ($XIX_OIA / $XIX_OIL) =="
"$XIX_OIA" -o "$BASE.xix.oi" "$BASE.s"
"$XIX_OIL" -E "$ENTRY" -o "$BASE.xix.out" "$BASE.xix.oi"

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

if command -v qemu-riscv32 >/dev/null 2>&1; then
    echo "== running under qemu-riscv32 =="
    # a test program's exit code is often intentionally non-zero --
    # don't let `set -e` treat that as a script failure.
    echo "-- goken --"
    set +e
    qemu-riscv32 "./$BASE.goken.out"; GOKEN_RC=$?
    set -e
    echo "(exit code: $GOKEN_RC)"
    echo "-- xix --"
    set +e
    qemu-riscv32 "./$BASE.xix.out"; XIX_RC=$?
    set -e
    echo "(exit code: $XIX_RC)"
    if [ "$GOKEN_RC" = "$XIX_RC" ]; then
        echo "PASS: same exit code"
    else
        echo "FAIL: exit codes differ (goken=$GOKEN_RC xix=$XIX_RC)"
    fi
else
    echo "== qemu-riscv32 not found, skipping functional run ==" 1>&2
fi
