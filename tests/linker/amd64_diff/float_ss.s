// Single-precision mirror of float_sd.s -- exercises the same shapes
// (MOVSS reg<->reg/mem, ADDSS/SUBSS/MULSS/DIVSS, UCOMISS, CVTSQ2SS/
// CVTTSS2SQ), confirming the `A.floatp_precision`-threaded design
// (see Ast_asm6.ml's MovF/ArithF/CmpF/CvtIntToF/CvtFToInt comments and
// Codegen6.ml's `sse_prefix`) correctly swaps the `0xf2`/`0xf3` prefix
// throughout -- except UCOMISS, which (unlike UCOMISD's own `0x66`)
// has *no* legacy prefix byte at all in real amd64 (confirmed against
// real 6a/6l: "UCOMISS X1,X0" -> `0f 2e c1`, "UCOMISD X1,X0" ->
// `66 0f 2e c1` -- see Codegen6.ml's `ucomis_prefix`).
//
// Same self-check as float_sd.s, at single precision: (10.0+3.0)*2.0
// - 2.0) / 2.0 = 12.0, truncated back to an int (AX=12), then MOVSS
// round-trips that same 12.0 through memory, combined with a
// separately-converted 1.0+12.0=13.0 compared via UCOMISS (13.0 >
// 12.0, via the existing unsigned JHI condition) for a final exit
// code of 12.
TEXT _start(SB), $0
	MOVQ	$10, AX
	CVTSQ2SS	AX, X0      // X0 = 10.0f
	MOVQ	$3, BX
	CVTSQ2SS	BX, X1      // X1 = 3.0f
	ADDSS	X1, X0              // X0 = 13.0f
	MOVQ	$2, CX
	CVTSQ2SS	CX, X2      // X2 = 2.0f
	MULSS	X2, X0               // X0 = 26.0f
	SUBSS	X2, X0               // X0 = 24.0f
	DIVSS	X2, X0               // X0 = 12.0f
	CVTTSS2SQ	X0, AX      // AX = 12

	MOVSS	X0, -8(SP)           // exercise MOVSS reg->mem
	MOVQ	$1, DX
	CVTSQ2SS	DX, X3
	MOVSS	-8(SP), X4           // exercise MOVSS mem->reg -- X4 = 12.0f
	ADDSS	X3, X4               // X4 = 13.0f
	UCOMISS	X0, X4               // X4(13.0) vs X0(12.0) -> X4 > X0
	JHI	greater
	MOVQ	$99, AX
greater:

	MOVQ	AX, DI               // exit(12)
	MOVQ	$60, AX
	SYSCALL
