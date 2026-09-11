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
    "tests/linker/mips_diff/movw_andcon_mips.s:_start"
    "tests/linker/mips_diff/movbh_mips.s:_start"
    "tests/linker/mips_diff/case16_mips.s:_start"
    "tests/linker/mips_diff/mullohi_mips.s:_start"
    "tests/linker/mips_diff/case22_mips.s:_start"
    "tests/linker/mips_diff/movw_ucon_mips.s:_start"
    "tests/linker/mips_diff/case23_25_mips.s:_start"
    "tests/linker/mips_diff/lacon_mips.s:_start"
    "tests/linker/mips_diff/case32_33_mips.s:_start"
    "tests/linker/mips_diff/case30_31_mips.s:_start"
    "tests/linker/mips_diff/case34_mips.s:_start"
    "tests/linker/mips_diff/case7_8_mips.s:_start"
)
# tests/linker/mips_diff/call_mips.s, case6_mips.s, immcon_mips.s,
# movbh_check_mips.s, mullohi_check_mips.s, case30_31_check_mips.s,
# case34_check_mips.s and case7_8_check_mips.s are deliberately NOT
# in this list: all eight are functionally correct (matching
# qemu-mips exit codes) but not byte-identical, because goken's
# sched.c hoists real instructions into branch/call delay slots
# (and, separately, pads NOPs around certain MUL-result HI/LO
# read/write transitions, MTC1/MFC1 COP1-transfer read/write
# transitions, and plain load-delay-slot reads, i.e. case 8/36)
# instead of what this port emits -- see
# docs/claude_notes/todo_mips_port.org. immcon_mips.s and
# movbh_check_mips.s use BEQ/JMP purely to self-check their own
# arithmetic, so they inherit the same scheduler-only diff as
# case6_mips.s (which has more branches than call_mips.s, hence a
# bigger diff -- 16 bytes across several delay slots, not just 4);
# mullohi_check_mips.s, case30_31_check_mips.s, case34_check_mips.s
# and case7_8_check_mips.s each inherit *both* that BEQ/JMP gap
# *and* their own respective hazard's NOP-padding gap (see
# mullohi_mips.s's, case30_31_mips.s's, case34_mips.s's and
# case7_8_mips.s's own comments for what's byte-identical there
# instead). The root cause and the decision not to port the
# scheduler are identical in all eight. Run any of them manually
# with scripts/diff-mips.sh to see their (small, known,
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
