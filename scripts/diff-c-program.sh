#!/bin/bash
# Differential *program*-level stress test: find a real C program's
# real minimal lib_core/libc dependency closure, assemble+link it with
# xix's own oNa/oNl (best-effort, working around known separate gaps),
# and run the result under qemu-user -- comparing against goken's own
# real Nc/Na/Nl only informationally, since byte/behavior parity with
# goken isn't the bar here (see scripts/find-c-closure.py's own header
# for why: some xix-only extensions, like IndirectShift/CASE/BCASE,
# have no real 5a grammar at all).
#
# This is a thin wrapper around the two Python tools that actually do
# the work:
#   scripts/find-c-closure.py  -- BFS the real closure (not goken's
#                                  full ~140-file "kitchen sink") via
#                                  goken's real Nc -S output, including
#                                  the mandatory arch/$objtype/rt0.s +
#                                  port/mainargs.c roots a downward-only
#                                  BFS can never discover on its own.
#   scripts/build-c-program.py -- assemble+link+run the closure with
#                                  xix's own tools, working around two
#                                  known, separate, documented gaps
#                                  (MOVFD/MOVDF, non-chipfloat FPA
#                                  constants) so a closure that merely
#                                  *contains* dead code needing them
#                                  doesn't block a real, working link.
#
# Usage:
#   ./scripts/diff-c-program.sh <arch> <main.c> [entry_symbol]
#
# For the --out FILE option (save the final linked binary somewhere
# durable), call scripts/build-c-program.py directly on this script's
# own closure output, or just add --out here after entry_symbol -- see
# below, it's passed straight through.
#
# <arch> is one of: 5 (arm), 6 (amd64), 7 (arm64), v (mips),
# i (riscv32), j (riscv64) -- see find-c-closure.py's own ARCH_TABLE.
# Only '5' is actually verified end-to-end so far (see
# docs/claude_notes/plan_hello_libc_linking.md); the others are wired
# the same way but each needs its own goken Nc's -S output checked for
# the same comma-padding print bug ARM's 5c had (fixed at the source,
# ~/goken/compilers/5c/list.c's Pconv) before they can work at all.
#
# Env:
#   GOKEN_ROOT   path to a built goken checkout (default: ~/goken)

set -e

ARCH=$1
CFILE=$2
ENTRY=${3:-_main}
# claude: everything from $4 onward (e.g. "--out FILE") is passed
# straight through to build-c-program.py -- $# is guaranteed >= 2 by
# the check just below, but may be < 3 (ENTRY defaulted, nothing to
# shift past it), so shift exactly min(3, $#) rather than a fixed 3
# (a plain "shift 3" errors out under `set -e` when only 2 args were
# given).
shift "$(( $# < 3 ? $# : 3 ))"

if [ -z "$ARCH" ] || [ -z "$CFILE" ]; then
    echo "usage: $0 <arch: 5|6|7|v|i|j> <main.c> [entry_symbol] [--out FILE]" 1>&2
    exit 2
fi
if [ ! -f "$CFILE" ]; then
    echo "$0: no such file: $CFILE" 1>&2
    exit 2
fi

GOKEN_ROOT=${GOKEN_ROOT:-$HOME/goken}
if [ ! -d "$GOKEN_ROOT" ]; then
    echo "$0: GOKEN_ROOT not found ($GOKEN_ROOT); skipping" 1>&2
    exit 0
fi

XIX_ROOT=$(cd "$(dirname "$0")/.." && pwd)
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT

echo "== finding the real dependency closure for $CFILE =="
GOKEN_ROOT="$GOKEN_ROOT" python3 "$XIX_ROOT/scripts/find-c-closure.py" \
    "$ARCH" "$CFILE" --out-dir "$TMP/closure"

echo
echo "== assembling+linking the closure with xix, running under qemu =="
GOKEN_ROOT="$GOKEN_ROOT" python3 "$XIX_ROOT/scripts/build-c-program.py" \
    "$ARCH" "$TMP/closure" "$ENTRY" "$@"
