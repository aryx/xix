#!/bin/bash
# Differential test driver for the MIPS port. Mirrors test-arm.sh --
# see docs/claude_notes/notes_mips_port_plan.txt and
# docs/claude_notes/todo_mips_port.org.
#
# Usage: ./test-mips.sh

set -e

cd "$(dirname "$0")"

# file:entry_symbol pairs -- entry defaults to _main (the linkers'
# default) when not TEXT _start.
CASES=(
    "tests/linker/mips_diff/hello_linux_mips.s:_start"
    "tests/linker/mips_diff/exit_linux_mips.s:_start"
    "tests/linker/mips_diff/addr_mips.s:_start"
    "tests/linker/mips_diff/kitchen_sink_mips.s:_start"
    "tests/linker/mips_diff/case2_mips.s:_start"
    "tests/linker/mips_diff/case9_mips.s:_start"
)
# tests/linker/mips_diff/call_mips.s and case6_mips.s are
# deliberately NOT in this list: both are functionally correct
# (matching qemu-mips exit codes) but not byte-identical, because
# goken's sched.c hoists real instructions into branch/call delay
# slots instead of the plain NOPs this port emits -- see
# docs/claude_notes/todo_mips_port.org. case6_mips.s has more
# branches than call_mips.s so the diff is bigger (16 bytes across
# several delay slots, not just 4), but the root cause and the
# decision not to port the scheduler are identical. Run either
# manually with scripts/diff-mips.sh to see their (small, known,
# scheduler-only) diffs.

FAIL=0
for c in "${CASES[@]}"; do
    file=${c%%:*}
    entry=${c##*:}
    echo "### $file (entry $entry)"
    if ! ./scripts/diff-mips.sh "$file" "$entry"; then
        FAIL=1
    fi
    echo
done

if [ "$FAIL" -ne 0 ]; then
    echo "test-mips.sh: some comparisons failed" 1>&2
    exit 1
fi
