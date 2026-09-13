// Exercises R8-R15 (the extended registers, needing REX.R/.B) across
// every instruction wired so far -- MOVQ $imm,R8/R9 (REX.B on the
// ModRM.rm side), ADDQ R8,R9 (REX.R *and* REX.B at once, one on each
// side of the same ModRM byte), CMPQ R9,$imm (REX.B), MOVQ R9,AX
// (REX.R, no REX.B since AX<8) -- see Codegen6.ml's `rex`/
// `rex_b_of_resolved_gen` comment. Every individual REX byte here was
// confirmed against goken's real 6a byte output before writing this
// as a committed fixture.
//
// Self-check: R9 = 9 + 3 = 12; CMPQ+JEQ verifies that, then exits with
// it. Wrong REX bits on any instruction above would either corrupt
// which register is actually read/written (wrong result) or which
// register CMPQ's own comparison secretly targets (JEQ not taken,
// exit 999 instead).
TEXT _start(SB), $0
	MOVQ	$9, R8
	MOVQ	$3, R9
	ADDQ	R8, R9          // R9 = 9 + 3 = 12
	CMPQ	R9, $12
	JEQ	ok
	MOVQ	$999, R9
ok:
	MOVQ	R9, DI          // exit(12)
	MOVQ	$60, AX
	SYSCALL
