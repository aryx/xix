#!/bin/bash
# Differential test: assemble+link a .s file with both goken's va/vl
# (the C Plan 9 reference) and xix's ova/ovl, then compare the final
# ELF executables byte-for-byte and (when qemu-mips is available)
# run both under it.
#
# Mirrors scripts/diff-arm.sh -- see docs/claude_notes/notes_arm_port_plan.txt
# for why only the final executable is ever compared, never the
# intermediate object files.
#
# Usage:
#   ./scripts/diff-mips.sh tests/linker/mips_diff/hello_linux_mips.s [entry_symbol]
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
GOKEN_VA="$GOKEN_BIN/va"
GOKEN_VL="$GOKEN_BIN/vl"
if [ ! -x "$GOKEN_VA" ] || [ ! -x "$GOKEN_VL" ]; then
    echo "$0: goken va/vl not found under $GOKEN_BIN (build goken first); skipping" 1>&2
    exit 0
fi

XIX_ROOT=$(cd "$(dirname "$0")/.." && pwd)
# Prefer this repo's freshly-built binaries over whatever ova/ovl
# might be on PATH -- we're porting/testing this checkout, not some
# other installed version.
XIX_OVA="$XIX_ROOT/_build/default/bin_dune/ova"
XIX_OVL="$XIX_ROOT/_build/default/bin_dune/ovl"
if [ ! -x "$XIX_OVA" ]; then XIX_OVA=$(command -v ova || true); fi
if [ ! -x "$XIX_OVL" ]; then XIX_OVL=$(command -v ovl || true); fi
if [ ! -x "$XIX_OVA" ] || [ ! -x "$XIX_OVL" ]; then
    echo "$0: xix ova/ovl not found (run 'dune build' first)" 1>&2
    exit 2
fi

TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT

BASE=$(basename "$SFILE" .s)
cp "$SFILE" "$TMP/$BASE.s"
cd "$TMP"

echo "== assembling+linking with goken ($GOKEN_VA / $GOKEN_VL) =="
"$GOKEN_VA" -o "$BASE.goken.v" "$BASE.s"
# -s: strip goken's native Plan9 symbol/debug table -- it embeds the
# invocation cwd and source path, so it's not byte-reproducible and
# xix never emits one anyway (see notes_arm_port_plan.txt). No -H
# needed: vl already defaults to ELF (HEADTYPE=7), unlike 5l.
"$GOKEN_VL" -E "$ENTRY" -s -o "$BASE.goken.out" "$BASE.goken.v"

echo "== assembling+linking with xix ($XIX_OVA / $XIX_OVL) =="
"$XIX_OVA" -o "$BASE.xix.ov" "$BASE.s"
"$XIX_OVL" -E "$ENTRY" -o "$BASE.xix.out" "$BASE.xix.ov"

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

if command -v qemu-mips >/dev/null 2>&1; then
    echo "== running under qemu-mips =="
    # a test program's exit code is often intentionally non-zero --
    # don't let `set -e` treat that as a script failure.
    echo "-- goken --"
    set +e
    qemu-mips "./$BASE.goken.out"; GOKEN_RC=$?
    set -e
    echo "(exit code: $GOKEN_RC)"
    echo "-- xix --"
    set +e
    qemu-mips "./$BASE.xix.out"; XIX_RC=$?
    set -e
    echo "(exit code: $XIX_RC)"
    if [ "$GOKEN_RC" = "$XIX_RC" ]; then
        echo "PASS: same exit code"
    else
        echo "FAIL: exit codes differ (goken=$GOKEN_RC xix=$XIX_RC)"
    fi
else
    echo "== qemu-mips not found, skipping functional run ==" 1>&2
fi
