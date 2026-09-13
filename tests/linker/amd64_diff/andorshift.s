// Exercises AND/OR (goken's yxorl/yxorb tables are shared verbatim
// with ADD/SUB/XOR, see Codegen6.ml's arith_ext/arith_rr_opcode),
// SHL(=SAL)/SHR/SAR (a new instruction family: shift-by-1, shift-by-
// immediate-N, and shift-by-CX all have genuinely different real x86
// encodings, see Ast_asm6.ml's Shift comment), and the Zil_/Zilo_m
// (opcode 0x81) imm32 arith form this port had deferred since its
// first checkpoint -- surfaced as a real, immediate need the moment a
// realistic AND-mask (0xFF, bigger than imm8's signed range) was
// tried, exactly the kind of pattern real 6c-generated code uses
// (confirmed against goken's own compiled output, e.g. vlrt.c's
// "ANDL $2047,CX"). Also exercises the Zil_/Z_il AX-implicit-opcode
// special case both Arith and Cmp's own imm32 forms have (mirroring
// the byte-width AL-special-case from an earlier checkpoint, one
// level up: opcode family (ext<<3)|0x05, confirmed against real 6a/6l
// "ANDL $0xFF,AX" -> "25 ff 00 00 00", no ModRM at all).
//
// Self-check: (0xF0 & 0x3C) | 0x05 = 0x35 (53), shifted left by CX=2
// then right by 2 (round-trips back to 53), plus -8 arithmetic-
// shifted right by 1 (-4, sign-preserving) added in (49), masked with
// a real imm32 AND (0xFF, a no-op here but exercises the Zilo_m/Zil_
// path), shifted left by 1 (98), then ORB $1 (99) for the final exit
// code.
TEXT _start(SB), $0
	MOVQ	$0xF0, AX
	ANDQ	$0x3C, AX       // AX = 0x30
	MOVQ	$0x05, BX
	ORQ	BX, AX          // AX = 0x35 (53)
	MOVQ	$2, CX
	SHLQ	CX, AX          // AX = 0x35 << 2 = 212 (shift-by-CX)
	SHRQ	$2, AX          // AX = 53 (shift-by-immediate-N)
	MOVQ	$-8, DX
	SARQ	$1, DX          // DX = -4 (shift-by-1, arithmetic)
	ADDQ	DX, AX          // AX = 53 + (-4) = 49
	ANDL	$0xFF, AX       // Zil_ (AX-implicit imm32), no-op mask
	SHLL	$1, AX          // AX = 98
	ORB	$1, AX          // AX = 99
	MOVQ	AX, DI          // exit(99)
	MOVQ	$60, AX
	SYSCALL
