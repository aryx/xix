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
)

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
