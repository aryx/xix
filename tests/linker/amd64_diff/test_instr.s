// Exercises TEST (goken's ytestl/ytestb tables) -- a real instruction
// found directly in goken's own hand-written amd64 assembly
// (lib_core/libc/os/windows/winio_amd64.s's own "TESTL AX,AX" idiom
// checking a Win32 BOOL result). A third distinct operand role-order
// from both Arith's ("op imr,gen") and Cmp's ("CMPQ gen,imr"): "TESTQ
// Rs,gen" puts the *register* written first into ModRM.reg -- see
// Ast_asm6.ml's Test comment, confirmed against real 6a/6l ("TESTL
// BX,CX" -> "85 d9": reg=BX, rm=CX).
//
// Self-check: 0 TESTQ'd against itself is zero (ZF=1, JEQ taken);
// 5 & 3 = 1 (nonzero, JNE taken); 8 & 8 = 8 (nonzero, via R8-R15) --
// all three checks passing reaches the final exit code, 77.
TEXT _start(SB), $0
	MOVQ	$0, AX
	TESTQ	AX, AX
	JEQ	zero
	MOVQ	$1, DI
	MOVQ	$60, AX
	SYSCALL
zero:
	MOVQ	$5, BX
	MOVQ	$3, CX
	TESTL	BX, CX          // BX & CX = 5 & 3 = 1, nonzero -> ZF=0
	JNE	nonzero
	MOVQ	$2, DI
	MOVQ	$60, AX
	SYSCALL
nonzero:
	MOVQ	$8, R9
	MOVQ	$8, R10
	TESTQ	R9, R10         // 8 & 8 = 8, nonzero (R8-R15)
	JNE	ok
	MOVQ	$3, DI
	MOVQ	$60, AX
	SYSCALL
ok:
	MOVQ	$77, DI
	MOVQ	$60, AX
	SYSCALL
