// A minimal CAS (compare-and-swap) primitive, matching the real shape
// found directly in goken's own tests/c/float/linux_amd64.s (`cas()`).
// Exercises LOCK (goken's own real, standalone-pseudo-instruction
// prefix, opcode 0xf0 alone), CMPXCHGL (real x86's atomic compare-
// and-exchange -- compares AX against the memory operand; equal ->
// memory := src, ZF=1; unequal -> AX := memory, ZF=0), JZ (a real,
// hand-maintained alternate spelling for JEQ in goken's own lexer),
// and a register *other than SP* used as a plain memory base
// ("0(BX)") -- a genuinely separate addressing gap this port's own
// `encode_rm` didn't have wired at all before this checkpoint (only
// SP was), found by testing this exact function.
//
// Self-check: CAS(slot, 999, 9) must fail (999 != slot's initial 5)
// and leave slot unchanged at 5; CAS(slot, 5, 9) must then succeed,
// leaving slot=9 and returning 1. slot(9) + cas-result(1) = 10, the
// final exit code.
TEXT cas(SB), $0
	MOVQ	8(SP), BX	// val pointer
	MOVL	16(SP), AX	// old
	MOVL	20(SP), CX	// new
	LOCK
	CMPXCHGL	CX, 0(BX)
	JZ	cas_ok
	MOVL	$0, AX
	RET
cas_ok:
	MOVL	$1, AX
	RET

TEXT _start(SB), $0
	SUBQ	$24, SP
	LEAQ	slot(SB), BX
	MOVQ	BX, 0(SP)
	MOVL	$999, 8(SP)  // old = 999 (wrong -- slot starts at 5)
	MOVL	$9, 12(SP)   // new = 9
	CALL	cas(SB)      // should FAIL (returns 0), slot stays 5
	ADDQ	$24, SP
	CMPL	AX, $0
	JEQ	failed_ok
	MOVQ	$1, DI       // wrong: CAS should have failed
	MOVQ	$60, AX
	SYSCALL
failed_ok:
	MOVL	slot(SB), CX
	CMPL	CX, $5       // slot must be unchanged
	JEQ	unchanged_ok
	MOVQ	$2, DI
	MOVQ	$60, AX
	SYSCALL
unchanged_ok:

	SUBQ	$24, SP
	LEAQ	slot(SB), BX
	MOVQ	BX, 0(SP)
	MOVL	$5, 8(SP)    // old = 5 (correct)
	MOVL	$9, 12(SP)   // new = 9
	CALL	cas(SB)      // should SUCCEED (returns 1), slot becomes 9
	ADDQ	$24, SP
	MOVL	slot(SB), CX
	ADDL	AX, CX       // CX = 9 + 1 = 10

	MOVL	CX, DI       // exit(10)
	MOVQ	$60, AX
	SYSCALL

GLOBL	slot(SB), $8
DATA	slot(SB)/8, $5
