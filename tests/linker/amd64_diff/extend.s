// Exercises the sign/zero-extending "widening move" family
// (MOVBLSX/MOVBLZX/MOVWLSX/MOVWLZX/MOVLQSX/MOVLQZX), a new
// instruction family confirmed genuinely high-frequency in real
// 6c-compiled output (MOVBLZX alone: 5 occurrences across just 4
// sampled tests/c/*.c files -- this whole family is how 6c implements
// C's integer-promotion rules). See Ast_asm6.ml's own Extend comment
// for two real quirks confirmed against real 6a/6l along the way:
// byte-sized sources need the same byte-register REX-forcing quirk
// B_-width arithmetic already established (word/long sources don't),
// and MOVLQZX -- despite being byte-for-byte a plain MOVL in real
// amd64 (a 32-bit register write already implicitly zero-extends to
// 64 bits) -- can't just alias onto this port's own `Move` codegen,
// since its own goken table entry is *always* the load-direction
// encoding while `Move`'s own reg-reg case picks the store direction
// first (a real difference caught by testing the reg-reg shape
// directly, not by the "it's just MOVL" reasoning alone).
//
// (Note: this fixture avoids "$0xFFFFFFF6"-style large positive hex
// literals for MOVQ -- a real, separate gap in this port's own Move-
// immediate range guards was found while first writing this fixture,
// see plan_amd64_port.md's "Real bugs/quirks"/phase-plan entries;
// "$-10" sidesteps it while still exercising the same sign-extension
// behavior.)
//
// Self-check: -10 (AL=0xF6) sign-extended is -10, zero-extended is
// 246; -1 (DX's low word 0xFFFF) sign-extended is -1, zero-extended
// is 65535; -1 (R8's low long 0xFFFFFFFF) sign-extended is -1,
// zero-extended is 4294967295 (none of the *Q results are used in the
// final arithmetic, just confirmed to assemble+encode correctly).
// -10 + 246 = 236 is the final exit code.
TEXT _start(SB), $0
	MOVQ	$-10, AX         // low byte 0xF6 = -10 as signed byte
	MOVBLSX	AX, BX           // BX = -10 (sign-extended from AL)
	MOVBLZX	AX, CX           // CX = 0xF6 = 246 (zero-extended from AL)
	MOVQ	$-1, DX
	MOVWLSX	DX, SI           // SI = -1 (sign-extended from DX's low word)
	MOVWLZX	DX, DI           // DI = 0xFFFF = 65535
	MOVQ	$-1, R8
	MOVLQSX	R8, R9           // R9 = -1 (sign-extended from R8's low long)
	MOVLQZX	R8, R10          // R10 = 0xFFFFFFFF = 4294967295

	ADDL	BX, CX          // CX = -10 + 246 = 236
	MOVBLZX	CX, CX          // keep low byte: 236 & 0xFF = 236 (already fits)
	MOVQ	CX, DI           // exit(236)
	MOVQ	$60, AX
	SYSCALL
