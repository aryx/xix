// Exercises single-operand MUL/IMUL/DIV/IDIV (goken's own ydivl/ydivb
// tables, implicit AX(:DX) shape), IMUL's own 2-operand form (goken's
// yimul table, opcode 0x0f 0xaf, ModRM.reg=dst/ModRM.rm=src), and
// CDQ/CQO (sign-extend AX into DX:AX before a signed divide) --
// genuinely more involved than every other instruction family in this
// port so far (fixed-register operands, DX:AX/RDX:RAX treated as one
// wide dividend/product), confirmed real but lower-frequency in
// sampled 6c output than AND/OR/shift/extend.
//
// Self-check: 100/7 = 14 remainder 2 (IDIVQ); 14*6=84, *9=756 (IMULL's
// own 2-operand form, once with a plain register and once with R9 to
// exercise R8-R15 in the rm/source role); 756/1000 = 0 remainder 756
// (IDIVL); 756*3=2268 (MULL, unsigned single-operand form) whose low
// byte (0xDC) is 220 -- the final exit code.
TEXT _start(SB), $0
	MOVQ	$100, AX
	MOVQ	$7, BX
	CQO
	IDIVQ	BX          // AX = 14 (quotient), DX = 2 (remainder)

	MOVL	$6, CX
	IMULL	AX, CX      // CX = 14 * 6 = 84 (2-operand form)

	MOVQ	$9, R9
	IMULL	R9, CX      // CX = 84 * 9 = 756 (R8-R15 in the rm role)

	MOVQ	$1000, SI
	MOVQ	CX, AX
	CDQ
	IDIVL	SI          // AX = 0 (quotient), DX = 756 (remainder)

	MOVL	DX, AX      // AX = 756 (use the remainder)
	MOVL	$3, DI
	MULL	DI          // AX = 756 * 3 = 2268, DX = 0

	ANDL	$0xFF, AX   // low byte of 2268 = 0xDC = 220
	MOVQ	AX, DI      // exit(220)
	MOVQ	$60, AX
	SYSCALL
