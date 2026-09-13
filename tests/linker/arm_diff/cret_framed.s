// A *conditional* RET inside a framed (non-leaf) procedure -- e.g.
// fmt/dofmt.c's "if(...) return ...;" compiled with locals allocated.
// Complements cret_leaf.s (autosize == 0): here TEXT declares $4 of
// locals, so the linker's noops() (goken's linkers/5l/noop.c) must
// insert a real save/restore frame and expand RET.MI into
// "MOVW.P.MI autosize(R13), R15" instead of the leaf case's bare
// "B.MI (R14)".
//
// Deliberately never touches the local slot at 0(R13): an earlier
// scratch version of this fixture stored a value there, aliasing the
// address the linker-inserted prologue had just saved R14 to (the
// prologue's "MOVW.W R14, -autosize(R13)" writes R14 at offset 0 of
// the *new* R13, same as a hand-written "0(R13)" reference) --
// that self-inflicted bug, not a real 5a/5l difference, was
// responsible for an earlier false lead where this case looked
// unimplementable (see docs/claude_notes/plan_hello_libc_linking.md).
// The $4 alone is enough to force autosize > 0 and exercise the
// framed epilogue; the fixture just never reads/writes that slot.
//
// Exercises both outcomes of the predicate: abs_framed(-42) takes the
// RET.MI path (via the framed CRET expansion this fixture targets),
// abs_framed(7) falls through to the plain, already-validated
// unconditional framed RET. Exit code 49 (42+7) if both are correct.

TEXT abs_framed(SB), $4
	CMP	$0, R0
	RSB.MI	$0, R0, R0
	RET.MI
	RET

TEXT _start(SB), $0
	MOVW	$-42, R0
	BL	abs_framed(SB)
	MOVW	R0, R4

	MOVW	$7, R0
	BL	abs_framed(SB)
	ADD	R4, R0, R0

	MOVW	R0, R7
	MOVW	$1, R7
	SWI	$0
