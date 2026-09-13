#!/bin/bash
# Differential test: assemble+link a .s file with both goken's 6a/6l
# (the C Plan 9 reference) and xix's o6a/o6l, then compare the final
# ELF executables byte-for-byte and (when qemu-x86_64 is available)
# run both under it.
#
# See docs/claude_notes/amd64_port.md for why only the final
# executable is ever compared (never the intermediate object files:
# o6a's .o6 uses OCaml Marshal by design, not goken's object format),
# and docs/claude_notes/arm_port.md for the harness shape this mirrors.
#
# Usage:
#   ./scripts/diff-amd64.sh tests/linker/amd64_diff/foo.s [entry_symbol] [extra_linker_flags]
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
GOKEN_6A="$GOKEN_BIN/6a"
GOKEN_6L="$GOKEN_BIN/6l"
if [ ! -x "$GOKEN_6A" ] || [ ! -x "$GOKEN_6L" ]; then
    echo "$0: goken 6a/6l not found under $GOKEN_BIN (build goken first); skipping" 1>&2
    exit 0
fi

XIX_ROOT=$(cd "$(dirname "$0")/.." && pwd)
# claude: uses the top-level bin_dune/ symlink (-> _build/install/
# default/bin/), not _build/default/bin_dune/ like the other diff-*.sh
# scripts -- found empirically that the latter can go stale after a
# plain `dune build` (it's only refreshed reliably by the install
# machinery), while bin_dune/ always tracks the freshest Main.exe.
XIX_O6A="$XIX_ROOT/bin_dune/o6a"
XIX_O6L="$XIX_ROOT/bin_dune/o6l"
if [ ! -x "$XIX_O6A" ]; then XIX_O6A=$(command -v o6a || true); fi
if [ ! -x "$XIX_O6L" ]; then XIX_O6L=$(command -v o6l || true); fi
if [ ! -x "$XIX_O6A" ] || [ ! -x "$XIX_O6L" ]; then
    echo "$0: xix o6a/o6l not found (run 'dune build' first)" 1>&2
    exit 2
fi

TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT

BASE=$(basename "$SFILE" .s)
cp "$SFILE" "$TMP/$BASE.s"
cd "$TMP"

echo "== assembling+linking with goken ($GOKEN_6A / $GOKEN_6L) =="
"$GOKEN_6A" -o "$BASE.goken.6" "$BASE.s"
# -s: strip goken's native Plan9 symbol/debug table, same reasoning as
# scripts/diff-arm.sh. -H7: same explicit ELF header type goken's own
# mkfile uses for 6l (tests/s/hello_arch/mkfile), unlike 7l which
# defaults to it. -S: a real 6l-specific quirk found while bringing up
# this port -- goken's shared liblk/elf.c only emits section headers
# when debug['S'] is set (see linkers/liblk/elf.c's own `if
# (debug['S'])` guards); 5l/7l/8l apparently always take that path by
# some other means (confirmed empirically: their own diff scripts
# don't need -S and still get section headers from goken), but 6l
# genuinely needs it spelled out. Without -S, goken's own 6l silently
# writes e_shoff=0/shnum=0 (no section headers at all) while xix's o6l
# -- like every other arch's own linker here -- always emits them, so
# omitting -S here would make every fixture spuriously FAIL on a
# section-header-table difference that has nothing to do with either
# side's actual codegen.
"$GOKEN_6L" -H7 -E "$ENTRY" -s -S $EXTRA_FLAGS -o "$BASE.goken.out" "$BASE.goken.6"

echo "== assembling+linking with xix ($XIX_O6A / $XIX_O6L) =="
"$XIX_O6A" -o "$BASE.xix.o6" "$BASE.s"
"$XIX_O6L" -E "$ENTRY" $EXTRA_FLAGS -o "$BASE.xix.out" "$BASE.xix.o6"

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

if command -v qemu-x86_64 >/dev/null 2>&1; then
    echo "== running under qemu-x86_64 =="
    echo "-- goken --"
    set +e
    qemu-x86_64 "./$BASE.goken.out"; GOKEN_RC=$?
    set -e
    echo "(exit code: $GOKEN_RC)"
    echo "-- xix --"
    set +e
    qemu-x86_64 "./$BASE.xix.out"; XIX_RC=$?
    set -e
    echo "(exit code: $XIX_RC)"
    if [ "$GOKEN_RC" = "$XIX_RC" ]; then
        echo "PASS: same exit code"
    else
        echo "FAIL: exit codes differ (goken=$GOKEN_RC xix=$XIX_RC)"
    fi
else
    echo "== qemu-x86_64 not found, skipping functional run ==" 1>&2
fi
