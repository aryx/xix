#!/bin/bash
# Differential test: assemble+link a .s file with both goken's 7a/7l
# (the C Plan 9 reference) and xix's o7a/o7l, then compare the final
# ELF executables byte-for-byte and (when qemu-aarch64 is available)
# run both under it.
#
# See docs/claude_notes/arm64_port.md for why only the
# final executable is ever compared (never the intermediate object
# files: o7a's .o7 uses OCaml Marshal by design, not goken's object
# format), and docs/claude_notes/arm_port.md for the harness shape
# this mirrors.
#
# Usage:
#   ./scripts/diff-arm64.sh tests/linker/arm64_diff/foo.s [entry_symbol] [extra_linker_flags]
#
# Env:
#   GOKEN_ROOT   path to a built goken checkout (default: ~/goken)

set -e

SFILE=$1
ENTRY=${2:-_main}
EXTRA_FLAGS=${3:-}

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
GOKEN_7A="$GOKEN_BIN/7a"
GOKEN_7L="$GOKEN_BIN/7l"
if [ ! -x "$GOKEN_7A" ] || [ ! -x "$GOKEN_7L" ]; then
    echo "$0: goken 7a/7l not found under $GOKEN_BIN (build goken first); skipping" 1>&2
    exit 0
fi

# claude: prefer the repo-root bin_dune/ symlinks (bin_dune/o7a ->
# _build/default/assembler/Main.exe, one hop) over
# _build/default/bin_dune/ (dune's own copy of that same symlinked
# source directory) -- the latter is a real, reproducible staleness
# trap: dune doesn't reliably re-snapshot it on every incremental
# build (confirmed directly: after a clean `dune build`,
# _build/default/bin_dune/o7l's own md5sum can still be the PREVIOUS
# build's while _build/default/linker/Main.exe and bin_dune/o7l are
# both already the new one), so a test run right after an edit can
# silently exercise stale code. Found chasing down what looked like a
# fix not taking effect while debugging the local_param_offset bug.
XIX_ROOT=$(cd "$(dirname "$0")/.." && pwd)
XIX_O7A="$XIX_ROOT/bin_dune/o7a"
XIX_O7L="$XIX_ROOT/bin_dune/o7l"
if [ ! -x "$XIX_O7A" ]; then XIX_O7A=$(command -v o7a || true); fi
if [ ! -x "$XIX_O7L" ]; then XIX_O7L=$(command -v o7l || true); fi
if [ ! -x "$XIX_O7A" ] || [ ! -x "$XIX_O7L" ]; then
    echo "$0: xix o7a/o7l not found (run 'dune build' first)" 1>&2
    exit 2
fi

TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT

BASE=$(basename "$SFILE" .s)
cp "$SFILE" "$TMP/$BASE.s"
cd "$TMP"

echo "== assembling+linking with goken ($GOKEN_7A / $GOKEN_7L) =="
"$GOKEN_7A" -o "$BASE.goken.7" "$BASE.s"
# -s: strip goken's native Plan9 symbol/debug table, same reasoning as
# scripts/diff-arm.sh. 7l already defaults HEADTYPE to ELF on Linux
# (unlike 5l, which needs an explicit -H7), so no -H flag here.
"$GOKEN_7L" -E "$ENTRY" -s $EXTRA_FLAGS -o "$BASE.goken.out" "$BASE.goken.7"

echo "== assembling+linking with xix ($XIX_O7A / $XIX_O7L) =="
"$XIX_O7A" -o "$BASE.xix.o7" "$BASE.s"
"$XIX_O7L" -E "$ENTRY" $EXTRA_FLAGS -o "$BASE.xix.out" "$BASE.xix.o7"

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

if command -v qemu-aarch64 >/dev/null 2>&1; then
    echo "== running under qemu-aarch64 =="
    echo "-- goken --"
    set +e
    qemu-aarch64 "./$BASE.goken.out"; GOKEN_RC=$?
    set -e
    echo "(exit code: $GOKEN_RC)"
    echo "-- xix --"
    set +e
    qemu-aarch64 "./$BASE.xix.out"; XIX_RC=$?
    set -e
    echo "(exit code: $XIX_RC)"
    if [ "$GOKEN_RC" = "$XIX_RC" ]; then
        echo "PASS: same exit code"
    else
        echo "FAIL: exit codes differ (goken=$GOKEN_RC xix=$XIX_RC)"
    fi
else
    echo "== qemu-aarch64 not found, skipping functional run ==" 1>&2
fi
