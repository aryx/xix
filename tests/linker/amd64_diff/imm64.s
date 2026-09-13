// Exercises a true 64-bit immediate move (goken's Yi64/Ziq_rp row) --
// only reached when the value doesn't fit the sign-extended-32-bit
// Ys32 class MOVQ's own immediate form otherwise always uses (see
// Codegen6.ml's own comment on why Ziq_rp's further internal l==0/
// l==-1 special cases are provably unreachable for MOVQ specifically:
// Ys32 already catches those first). Opcode 0xb8+reg, REX.W set,
// followed by the full 8-byte immediate -- the same opcode-embedding
// family as MOVL's own Zil_rp, confirmed against real 6a for both a
// low register and an R8-R15 one.
//
// Self-check: two large constants (each needing the full 64 bits,
// confirmed >0x7fffffff so Ys32 can't catch them) are added together
// and truncated to a small, easily-verified exit code via a final
// 32-bit-view ADDL (0x1_0000_0055 + 0x2_0000_002A truncates to
// 0x7f = 127 once only the low 32 bits are kept, since ADDL zeroes the
// upper 32 bits of its own destination register on write).
TEXT _start(SB), $0
	MOVQ	$0x100000055, AX
	MOVQ	$0x200000029, R9
	ADDL	R9, AX          // 0x55+0x29 = 0x7e in the low 32 bits;
	                        // the >32-bit parts are dropped by ADDL's
	                        // own 32-bit write (upper 32 of AX zeroed)
	ADDL	$1, AX          // AX = 0x7f = 127
	MOVQ	AX, DI          // exit(127)
	MOVQ	$60, AX
	SYSCALL
