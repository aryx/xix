// Exercises BL (branch-and-link, i.e. a CALL) to a defined symbol
// through both the assembler and the linker. hello_arm.s only uses
// MOVW/SWI, so the BL-to-symbol relocation and the branch instruction
// encoding in 5l were never covered by the arm variants tests -- a gap
// that let a linker issue on BL hide until the full pi build. Here
// _start calls writemsg() with BL, which writes the string and RETs.
//
// (Aside found while adding this: linking a BL whose target is
// *undefined* prints a spurious "illegal combination BL" after the
// real "undefined:" error, with a different operand-class number in
// each lineage -- 26 for kencc vs 15 for principia. It only happens on
// the error path of an already-failing link, so it does not affect any
// valid object or executable; both lineages produce byte-identical
// output for defined targets, as this test checks.)

TEXT _start(SB), $0
	BL	writemsg(SB)
	MOVW	$0, R0
	MOVW	$1, R7
	SWI	$0

TEXT writemsg(SB), $0
	MOVW	$1, R0
	MOVW	$msg(SB), R1
	MOVW	$13, R2
	MOVW	$4, R7
	SWI	$0
	RET

GLOBL	msg(SB), $16
DATA	msg+0(SB)/8, $"Hello, w"
DATA	msg+8(SB)/5, $"orld\n"
