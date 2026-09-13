// Exercises double-precision SSE: MOVSD (reg<->reg, reg<->mem),
// ADDSD/SUBSD/MULSD/DIVSD, UCOMISD, and both directions of int<->float
// conversion (CVTSQ2SD/CVTTSD2SQ) -- see Ast_asm6.ml's own "Floating
// point" section and Codegen6.ml's comments for the real 6a/6l bytes
// each of these was confirmed against, in particular:
//  - MOVSD's own reg-reg move always takes the *load* opcode (0x10),
//    the opposite row-order choice from MOVQ/MOVL/MOVW/MOVB;
//  - UCOMISD's prefix is 0x66 (Pe), not 0xf2 (Pf2) like every other
//    instruction here;
//  - CVTSQ2SD/CVTTSD2SQ both mandatorily set REX.W (goken's "Pw"
//    alongside "Pf2"), unlike MOVSD/ADDSD/etc, which never do.
//
// No float-immediate support exists in real amd64 at all (confirmed:
// real 6a/6l rejects "MOVSD $0,X0" outright), so every double value
// here is built from an integer via CVTSQ2SD instead.
//
// Self-check: (10.0+3.0)*2.0 - 2.0) / 2.0 = 12.0, truncated back to an
// int (AX=12), then MOVSD round-trips that same 12.0 through memory,
// combined with a separately-converted 1.0+12.0=13.0 compared via
// UCOMISD (13.0 > 12.0, confirmed via the existing unsigned JHI
// condition -- see Ast_asm6.ml's CmpF comment for why no new condition
// type was needed) for a final exit code of 12.
TEXT _start(SB), $0
	MOVQ	$10, AX
	CVTSQ2SD	AX, X0      // X0 = 10.0
	MOVQ	$3, BX
	CVTSQ2SD	BX, X1      // X1 = 3.0
	ADDSD	X1, X0              // X0 = 13.0
	MOVQ	$2, CX
	CVTSQ2SD	CX, X2      // X2 = 2.0
	MULSD	X2, X0               // X0 = 26.0
	SUBSD	X2, X0               // X0 = 24.0
	DIVSD	X2, X0               // X0 = 12.0
	CVTTSD2SQ	X0, AX      // AX = 12

	MOVSD	X0, -8(SP)           // exercise MOVSD reg->mem
	MOVQ	$1, DX
	CVTSQ2SD	DX, X3
	MOVSD	-8(SP), X4           // exercise MOVSD mem->reg -- X4 = 12.0
	ADDSD	X3, X4               // X4 = 13.0
	UCOMISD	X0, X4               // X4(13.0) vs X0(12.0) -> X4 > X0
	JHI	greater
	MOVQ	$99, AX
greater:

	MOVQ	AX, DI               // exit(12)
	MOVQ	$60, AX
	SYSCALL
