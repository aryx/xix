// MOVW.S -- the classic ARM "test and move" idiom (set NZCV flags
// from the moved value, for a following predicated instruction like
// "MOVW.NE ..."). Unlike CASE/BCASE and the ".CC"/".CS" condition
// aliases elsewhere in this directory, this IS real, valid 5a syntax
// (confirmed directly against goken's own real 5a) -- found via real
// 5c -S output for lib_core/libc/fmt/dofmt.c's "MOVW.S R0,R7" ahead
// of a predicated "MOVW.NE ...", see
// docs/claude_notes/plan_hello_libc_linking.md.
//
// R0=5 (nonzero) -> MOVW.NE fires -> R0=11.
// R1=0 (zero)    -> MOVW.EQ fires -> R1=44.
// Exit code 11+44=55 if both conditional paths worked correctly.

TEXT _start(SB), $0
	MOVW	$5,R0
	MOVW.S	R0,R7
	MOVW.NE	$11,R0
	MOVW.EQ	$22,R0

	MOVW	$0,R1
	MOVW.S	R1,R7
	MOVW.NE	$33,R1
	MOVW.EQ	$44,R1

	ADD	R0,R1,R0
	MOVW	R0,R7
	MOVW	$1,R7
	SWI	$0
