// Exercises NEG/NOT/INC/DEC (goken's yincb/yincl/yincw/yscond tables)
// -- a new instruction family, but structurally identical to Shift's
// own shift-by-1/shift-by-CL cases (goken's Zo_m: opcode + ModRM with
// a fixed extension digit, no immediate at all) -- across Q/L/B width
// and R8-R15.
//
// Self-check: 5 -NEG-> -5 -NOT-> 4 -INC-> 5 -INC-> 6 -DEC-> 5 (AX);
// 100 -NEG-> -100 -NOT-> 99 (BX, 32-bit); 9 -INC-> 10 (R9, exercising
// REX.B); 0 -NOTB-> 255 (CX, byte width). 5+99+10-255 = -141, whose
// low byte (0x73) is 115 -- the final exit code.
TEXT _start(SB), $0
	MOVQ	$5, AX
	NEGQ	AX               // AX = -5
	NOTQ	AX               // AX = ~(-5) = 4
	INCQ	AX               // AX = 5
	INCQ	AX               // AX = 6
	DECQ	AX               // AX = 5

	MOVL	$100, BX
	NEGL	BX               // BX = -100
	NOTL	BX               // BX = 99

	MOVQ	$9, R9
	INCQ	R9               // R9 = 10 (R8-R15, REX.B)

	MOVQ	$0, CX
	NOTB	CX               // CX low byte = 0xFF = 255 (byte width)

	ADDL	BX, AX          // AX = 5 + 99 = 104
	ADDL	R9, AX          // AX = 104 + 10 = 114
	SUBL	CX, AX          // AX = 114 - 255 = -141
	ANDL	$0xFF, AX       // low byte of -141 = 0x73 = 115
	MOVQ	AX, DI
	MOVQ	$60, AX
	SYSCALL
