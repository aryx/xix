// Combined MIPS constructs test, mirroring tests/linker/arm_diff/
// kitchen_sink.s. Deliberately straight-line (no JAL): case 11's
// delay slot isn't byte-identical to goken yet (goken's sched.c
// hoists a real instruction from the call target instead of using
// a plain NOP -- see docs/claude_notes/todo_mips_port.org and
// call.s), and mixing that in here would make it harder to
// tell which gap a future byte diff is pointing at. This fixture's
// actual purpose is two GLOBLs at different offsets, the same setup
// that found ARM's BIG/immrot address-of-global bug.

TEXT _start(SB), $0
	MOVW	$setR30(SB), R30

	MOVW	$1, R4
	MOVW	$msgok(SB), R5
	MOVW	$3, R6
	MOVW	$4004, R2
	SYSCALL

	MOVW	$1, R4
	MOVW	$msgfail(SB), R5
	MOVW	$5, R6
	MOVW	$4004, R2
	SYSCALL

	MOVW	$0, R4
	MOVW	$4001, R2
	SYSCALL

GLOBL	msgok(SB), $4
DATA	msgok+0(SB)/3, $"OK\n"

GLOBL	msgfail(SB), $8
DATA	msgfail+0(SB)/5, $"FAIL\n"
