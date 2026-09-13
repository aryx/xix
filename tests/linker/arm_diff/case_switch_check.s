// Switch-statement jump-table dispatch (CASE/BCASE) -- see
// Ast_asm5.CASE/BCASE's own comment and
// docs/claude_notes/plan_hello_libc_linking.md for how this was found
// (stress-testing o5a/o5l against goken's real lib_core/libc, which
// needs this for e.g. fmt/dofmt.c and the strtol family). Unlike
// every other fixture in this directory, CASE/BCASE's concrete syntax
// is NOT real 5a syntax -- goken's own real 5a/7a grammar has no rule
// for it at all, since 5c never round-trips switch-statement code
// through the assembler's text parser (see the comment above for the
// full explanation). This fixture's byte-for-byte match against
// goken is still meaningful, though: it confirms xix's own expansion
// of this xix-only syntax produces the exact bytes goken's 5c/5l
// pipeline would have produced internally for the same switch
// statement -- verified by generating classify's body below directly
// from goken's real `5c -S` output for the equivalent C switch (see
// that investigation for the exact invocation), not hand-written.
//
// classify(2) should hit "case 2: return 12": exit code 12.
//
// _check (not byte-identical, per tests/linker/README.md): verified
// by direct comparison against goken's classify() compiled straight
// to a real object (5c, no -S text roundtrip) and linked with this
// same hand-written caller assembled by goken's own real 5a -- CASE
// itself disassembles byte-identical ("ldrls pc, [pc, r0, lsl #2]")
// and every BCASE table entry correctly resolves to its own case
// body's real address on each side. The only difference is the exact
// same dead-branch-chain-elimination gap tests/linker/arm_diff/
// movb_postindex_check.s already documents (goken's noop pass
// eliminates the initial "B 12(PC); B 20(PC)" redundant jump chain
// and reorders the case bodies accordingly; o5l doesn't), not
// anything specific to this fixture's CASE/BCASE support. Confirmed
// functionally identical: both exit 12.

TEXT _start(SB), $0
	MOVW	$2,R0
	BL	classify(SB)
	MOVW	R0,R7
	MOVW	$1,R7
	SWI	$0

TEXT classify(SB), $0
	MOVW	R0,R3
	B	12(PC)
	B	20(PC)
	MOVW	$10,R0
	RET
	MOVW	$11,R0
	RET
	MOVW	$12,R0
	RET
	MOVW	$13,R0
	RET
	MOVW	$-1,R0
	RET
	MOVW	R0,R2
	CMP	$3,R0
	CASE.LS	R0
	BHI	-5(PC)
	BCASE	-14(PC)
	BCASE	-13(PC)
	BCASE	-12(PC)
	BCASE	-11(PC)
	B	-10(PC)
	RET
