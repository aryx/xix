// Real x86 SIB scaled-index addressing -- "(BX)(CX*4)" (register
// base+index), "tab<>+0(SB)(CX*8)" (SB-relative global base, no base
// register at all in the SIB byte itself). Confirmed against goken's
// real assemblers/6a/a.y; genuinely common in real fmt/utf code for
// indexing an array by a loop variable (e.g. fmt/dofmt.c's own
// "LEAQ (BX)(CX*1),AX", fmt/fltfmt.c's own
// "MOVSD pows10<>+0(SB)(AX*8),X0").
TEXT	_start(SB), $0
	LEAQ	tab<>+0(SB),BX
	MOVQ	$2,CX
	MOVL	(BX)(CX*4),AX  // tab[2] = 30
	MOVQ	$1,DX
	MOVL	tab<>+0(SB)(DX*4),BX  // tab[1] = 20
	ADDL	BX,AX          // 30+20 = 50
	MOVQ	AX,DI
	MOVQ	$60,AX
	SYSCALL

	DATA	tab<>+0(SB)/4,$10
	DATA	tab<>+4(SB)/4,$20
	DATA	tab<>+8(SB)/4,$30
	DATA	tab<>+12(SB)/4,$40
	GLOBL	tab<>+0(SB),$16
