// MOVD/MOVF's "plain move" form -- a float-constant load from
// goken's 8-entry chipfloat table (real 5c -S output for e.g.
// fmt/fltfmt.c's "MOVD.NE $1.0,F0" or fmt/strtod.c's "MOVD $0.0,F1")
// or a register-to-register copy. Real, valid 5a syntax (confirmed
// against goken's real 5a). Shares asmout()'s case 54 with ArithF's
// dyadic ops (ADD_/SUB_/MUL_/DIV_), not case 55/76's int<->float
// conversion (MOVWF/MOVFW/MOVFD/MOVDF) -- see Codegen5.gop_movf's own
// comment. See docs/claude_notes/plan_hello_libc_linking.md.
//
// 1 != 2 sets NE, so MOVD.NE fires: F0 ends up 2.0, exit code 2.

TEXT _start(SB), $0
	MOVW	$1,R1
	CMP	$2,R1
	MOVD	$1.0,F0
	MOVD.NE	$2.0,F0
	MOVD	F0,F1
	MOVFW	F1,R0
	MOVW	$1,R7
	SWI	$0
