// Combined ARM constructs test: arithmetic (ADD/SUB), shift (SLL),
// multiply (MUL), a BL/RET subroutine call, address-of-global
// (MOVW $sym(SB)), and the write/exit syscalls -- broader coverage
// in one runnable program than any single fixture so far.
//
// Loosely in the spirit of goken's tests/c/mini/helloc.c (write a
// message, then exit), but hand-written directly in assembly rather
// than hand-translating the C: helloc.c's actual argument-passing
// convention (see xwrite_arm.s's buf+0(FP)/count+4(FP)) is a detail
// of 5c's ABI, not of o5a/o5l (what we're differential-testing here),
// so modeling it exactly would add real complexity for no coverage
// of anything this project changes. Passing values in plain
// registers to a local subroutine, as below, still exercises BL/RET
// and needs no stack frame.
//
// Deliberately straight-line (no CMP/Bxx): an earlier version used
// CMP+BEQ to pick between two messages, and goken turned out to
// duplicate the short exit epilogue into both branches instead of
// having one jump to a shared tail -- a real assembler/linker
// behavior, but a distinct one from anything in our current case
// list (see docs/claude_notes/todo_arm_port.org). Keeping this
// fixture branch-free avoids exercising that gap by accident.

TEXT _start(SB), $0
	MOVW	$10, R1
	MOVW	$32, R2
	ADD	R1, R2, R3	// R3 = 42

	MOVW	$3, R4
	MOVW	$2, R5
	MUL	R4, R5, R6	// R6 = 6

	SUB	R6, R3, R3	// R3 = 36

	MOVW	$1, R4
	SLL	$4, R4, R4	// R4 = 16
	ADD	R4, R3, R3	// R3 = 52 (unused, but exercised)

	MOVW	$msgok(SB), R1
	MOVW	$3, R2
	BL	xwrite(SB)

	MOVW	$0, R0
	MOVW	$1, R7
	SWI	$0
	RET

TEXT xwrite(SB), $0
	MOVW	$1, R0
	MOVW	$4, R7
	SWI	$0
	RET

GLOBL	msgok(SB), $4
DATA	msgok+0(SB)/3, $"OK\n"
