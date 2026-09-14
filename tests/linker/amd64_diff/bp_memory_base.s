// BP/R13 as an ordinary memory base -- real amd64's mod=00/rm=101
// means RIP-relative addressing, not "no displacement" (unlike every
// other register), so even a genuine offset-0 access through BP needs
// an explicit 1-byte $0 displacement (mod=01). Confirmed against real
// 6a/6l: "MOVBLZX (BP),AX" -> "0f b6 45 00" (mod=01, disp8=0x00), not
// the disp-less "0f b6 45" a naive mod=00 read would produce. Real
// 6c-compiled code genuinely reuses BP as a plain struct-pointer GPR
// (not a frame pointer) whenever a function doesn't need one -- e.g.
// fmt/fmtfd.c's own "MOVQ f+0(FP),BP; MOVB $0,(BP); MOVQ SI,8(BP)".
TEXT	_start(SB), $0
	LEAQ	buf<>+0(SB),BP
	MOVB	$42,(BP)
	MOVL	$7,8(BP)
	MOVBLZX	(BP),AX
	MOVL	8(BP),BX
	ADDL	BX,AX          // 42+7 = 49
	MOVQ	AX,DI
	MOVQ	$60,AX
	SYSCALL

	DATA	buf<>+0(SB)/8,$0
	GLOBL	buf<>+0(SB),$16
