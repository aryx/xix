// An address-of-global immediate ("$name(SB)") as a MOVQ source or a
// CMP second operand -- real 6a/6l resolves these to a fixed, link-
// time-constant absolute address (non-PIE). Confirmed against real
// fmt/fmt.c's own "MOVQ $fmtalloc<>+8(SB),DX" and "CMPQ
// DX,$fmtalloc<>+1032(SB)". Also exercises CMP's *reverse*-direction
// row ("CMPQ Rs,mem", goken's own ycmpl "Yrl,Yml,Zr_m" row) -- the
// opposite role order from CMP's own "CMPQ mem,Rs" (Zm_r) case, e.g.
// real fmt/dofmt.c's own "CMPL DX,n+8(FP)". Straight-line (no
// Jcc/JMP pair around the CMPQs) -- a "Jcc skip; JMP elsewhere" shape
// here would trip a real, separate, pre-existing gap (goken's own
// linker collapses that idiom into a single inverted-condition branch,
// which this port doesn't do -- unrelated to what this fixture
// checks). The CMPQs themselves are never branched on -- what's
// actually being checked is that the address computed by the MOVQ
// address-immediate is correct (dereferencing it via "MOVQ (AX),BX"
// and combining into the final exit code catches a wrong address).
// Also exercises both MOVQ's and CMPQ's own AX-special-case short
// encodings for an address-immediate (opcodes 0xb8/0x3d, the same
// "AX-implicit, imm32, no ModRM" shape their own `A.Int`-immediate
// siblings already use) -- byte-identical against real 6a/6l.
TEXT	_start(SB), $0
	MOVQ	$tab<>+8(SB),AX
	CMPQ	AX,$tab<>+8(SB) // equal -> ZF=1
	MOVQ	(AX),BX         // *(tab+8) = 99
	MOVQ	tab<>+0(SB),CX
	CMPQ	CX,tab<>+0(SB)  // reverse-direction row; equal -> ZF=1
	ADDQ	CX,BX           // 11+99 = 110
	MOVQ	BX,DI
	MOVQ	$60,AX
	SYSCALL

	DATA	tab<>+0(SB)/8,$11
	DATA	tab<>+8(SB)/8,$99
	GLOBL	tab<>+0(SB),$16
