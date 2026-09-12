// Exercises a conditional RET ("RET.MI") in a leaf procedure (no
// locals) -- real 5c compiles a predicated early-return (e.g.
// "if(x<0) return -x;") this way instead of a branch, confirmed
// against goken's real 5a (RET.MI assembles fine there). Before this
// fix, RET had zero condition support at all: ARM's own grammar only
// ever had `virtual_instr: TRET` (Ast_asm.virtual_instr.RET, no
// condition field possible since that type is shared across every
// arch). See Ast_asm5.ml's CRET comment for why this instead lives in
// ARM's own arch-specific `instr` type, RFE-style.
//
// Rewrite5.ml expands a leaf CRET into "ADD.cond $0,R14,PC" (case 6's
// own encoding, now threading the real condition instead of asserting
// AL -- see Codegen5.ml's B case comment). Framed (non-leaf) CRET is a
// separate, not-yet-matched case -- see Rewrite5.ml's own "raise Todo"
// there.
TEXT iabs(SB), $0
	CMP	$0, R0
	RSB.MI	$0, R0, R0
	RET.MI
	RET

TEXT _start(SB), $0
	// R0 = -42, built via RSB rather than "MOVW $-42,R0" directly --
	// negative-immediate encoding (MVN vs. this port's literal pool)
	// is a separate, pre-existing, already-known divergence (see
	// arm_port.md's MOV-immediate classification note) unrelated to
	// CRET; this sidesteps it the same way other fixtures do.
	MOVW	$42, R0
	RSB	$0, R0, R0
	BL	iabs(SB)
	MOVW	$1, R7
	SWI	$0
