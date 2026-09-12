// MIPS translation of goken's tests/s/variants/call_arm.s (no MIPS
// equivalent exists upstream in goken -- hand-written). Exercises
// JAL (case 11, branch-and-link, i.e. a CALL) to a defined symbol,
// and RET (case 18) returning from it -- also the first fixture
// where case 18's delay-slot fix runs from a JAL-called procedure
// rather than as the very last instruction of the program.

TEXT _start(SB), $0
	MOVW	$setR30(SB), R30
	JAL	writemsg(SB)

	MOVW	$0, R4
	MOVW	$4001, R2
	SYSCALL

TEXT writemsg(SB), $0
	MOVW	$1, R4
	MOVW	$msg(SB), R5
	MOVW	$13, R6
	MOVW	$4004, R2
	SYSCALL
	RET

GLOBL	msg(SB), $16
DATA	msg+0(SB)/8, $"Hello, w"
DATA	msg+8(SB)/5, $"orld\n"
