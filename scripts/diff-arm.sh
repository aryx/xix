#!/bin/bash
# Differential test: assemble+link a .s file with both goken's 5a/5l
# (the C Plan 9 reference) and xix's o5a/o5l, then compare the final
# ELF executables byte-for-byte and (when qemu-arm is available) run
# both under it.
#
# See docs/claude_notes/notes_arm_port_plan.txt for why only the final
# executable is ever compared (never the intermediate object files:
# o5a's .o5 uses OCaml Marshal by design, not goken's object format).
#
# Usage:
#   ./scripts/diff-arm.sh tests/linker/hello_linux_arm.s [entry_symbol]
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
GOKEN_5A="$GOKEN_BIN/5a"
GOKEN_5L="$GOKEN_BIN/5l"
if [ ! -x "$GOKEN_5A" ] || [ ! -x "$GOKEN_5L" ]; then
    echo "$0: goken 5a/5l not found under $GOKEN_BIN (build goken first); skipping" 1>&2
    exit 0
fi

XIX_ROOT=$(cd "$(dirname "$0")/.." && pwd)
# Prefer this repo's freshly-built binaries over whatever o5a/o5l
# might be on PATH (e.g. an older opam-installed copy) -- we're
# porting/testing this checkout, not some other installed version.
XIX_O5A="$XIX_ROOT/_build/default/bin_dune/o5a"
XIX_O5L="$XIX_ROOT/_build/default/bin_dune/o5l"
if [ ! -x "$XIX_O5A" ]; then XIX_O5A=$(command -v o5a || true); fi
if [ ! -x "$XIX_O5L" ]; then XIX_O5L=$(command -v o5l || true); fi
if [ ! -x "$XIX_O5A" ] || [ ! -x "$XIX_O5L" ]; then
    echo "$0: xix o5a/o5l not found (run 'dune build' first)" 1>&2
    exit 2
fi

TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT

BASE=$(basename "$SFILE" .s)
cp "$SFILE" "$TMP/$BASE.s"
cd "$TMP"

echo "== assembling+linking with goken ($GOKEN_5A / $GOKEN_5L) =="
"$GOKEN_5A" -o "$BASE.goken.5" "$BASE.s"
# -s: strip goken's native Plan9 symbol/debug table -- it embeds the
# invocation cwd and source path, so it's not byte-reproducible and
# xix never emits one anyway (see notes_arm_port_plan.txt).
"$GOKEN_5L" -H7 -E "$ENTRY" -s -o "$BASE.goken.out" "$BASE.goken.5"

echo "== assembling+linking with xix ($XIX_O5A / $XIX_O5L) =="
"$XIX_O5A" -o "$BASE.xix.o5" "$BASE.s"
"$XIX_O5L" -E "$ENTRY" -o "$BASE.xix.out" "$BASE.xix.o5"

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

if command -v qemu-arm >/dev/null 2>&1; then
    echo "== running under qemu-arm =="
    echo "-- goken --"
    qemu-arm "./$BASE.goken.out"; GOKEN_RC=$?
    echo "(exit code: $GOKEN_RC)"
    echo "-- xix --"
    qemu-arm "./$BASE.xix.out"; XIX_RC=$?
    echo "(exit code: $XIX_RC)"
else
    echo "== qemu-arm not found, skipping functional run ==" 1>&2
fi
