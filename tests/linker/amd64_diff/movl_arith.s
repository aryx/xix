// Exercises the 32-bit (L-suffixed) forms: MOVL immediate-to-register
// (goken's Zil_rp, "b8+reg"+imm32, *no* ModRM -- a genuinely different
// encoding shape from MOVQ's own Zilo_m/0xc7, see Codegen6.ml's own
// Move comment), MOVL immediate-to-memory (Zilo_m/0xc7, same shape as
// MOVQ minus REX.W), MOVL/ADDL/CMPL register forms (same opcodes as
// their Q-suffixed counterparts, but REX is entirely optional -- no
// prefix byte at all unless R8-R15 is involved, confirmed against
// real 6a output for every case here). Also exercises MOVL/ADDL/CMPL
// with an R8-R15 register, to confirm the L-suffixed REX.B computation
// matches the Q-suffixed one (`rex_opt`'s own `width` only changes the
// W bit, R/B logic is shared).
//
// Self-check: 100 + 23 = 123; verified with CMPL+JEQ, then stored to
// and reloaded from memory (0(SP)) to also exercise the memory-operand
// forms, then combined with an R8/R9 computation (9+3=12) for a final
// exit code of 123+12=135.
TEXT _start(SB), $0
	MOVL	$100, AX
	MOVL	$23, BX
	ADDL	BX, AX          // AX = 123
	CMPL	AX, $123
	JEQ	l_ok
	MOVL	$999, AX
l_ok:

	// claude: -8(SP)/-4(SP), not 0(SP)/4(SP): SP is a real, unadjusted
	// register here (see Ast_asm6.ml's own prelude -- _start has no
	// caller, and TEXT's $0 means no local frame is even nominally
	// reserved), so anything at/above SP belongs to the kernel-provided
	// argc/argv/envp block. The SysV red zone (128 bytes *below* SP)
	// is safe scratch space for a leaf sequence like this one.
	MOVL	AX, -8(SP)      // exercise MOVL reg->mem (Zr_m)
	MOVL	$0, AX          // clobber AX -- exercises MOVL's own Zclr
	                        // row (goken's ymovl *does* have a Yi0
	                        // row, same as ymovq; a real bug in this
	                        // port's own earlier assumption otherwise
	                        // was found and fixed via this exact line)
	MOVL	-8(SP), AX      // exercise MOVL mem->reg (Zm_r) -- AX = 123
	MOVL	$77, -4(SP)     // exercise MOVL imm->mem (Zilo_m)
	CMPL	-4(SP), $77     // CMPL's own immediate form is imm8-only so
	                        // far (see Codegen6.ml's own scope note) --
	                        // $77 fits, unlike the earlier $999 MOVL
	JEQ	mem_ok
	MOVL	$999, AX
mem_ok:

	MOVL	$9, R8
	MOVL	$3, R9
	ADDL	R8, R9          // R9 = 12
	CMPL	R9, $12
	JEQ	r_ok
	MOVL	$999, AX
r_ok:

	ADDL	R9, AX          // AX = 123 + 12 = 135 (32-bit add is fine,
	                        // result fits, upper 32 bits of RAX get
	                        // zeroed by the 32-bit write -- irrelevant
	                        // here since AX's own value already fits)
	MOVQ	AX, DI          // exit(135) -- widen via a real MOVQ store
	                        // (DI is read in full by SYSCALL, but AX's
	                        // own upper 32 bits are already zero after
	                        // any 32-bit write, per real x86-64 semantics)
	MOVQ	$60, AX
	SYSCALL
