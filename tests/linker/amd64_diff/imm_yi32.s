// Exercises Move's own newly-widened Yi32 immediate range (not just
// the narrower, sign-extendable Ys32 subset this port originally only
// handled) -- see Codegen6.ml's `fits_yi32` comment and
// plan_amd64_port.md's own "Real bugs/quirks" entry for the full
// story of how this gap was found.
//
// A real, subtle semantic quirk this fixture exists specifically to
// pin down: the *same* Yi32-classified immediate ("$0xFFFFFFF6")
// produces genuinely different 64-bit values depending on the
// destination -- a *register* destination goes through goken's own
// Ziq_rp "l==0" downgrade (opcode 0xb8+reg, no REX.W), which writes
// only the low 32 bits and *zero*-extends (real amd64's own ordinary
// 32-bit-write behavior); a *memory* destination goes through the
// general Zilo_m row (opcode 0xc7, REX.W set), whose 32-bit immediate
// operand *sign*-extends to fill the full 64-bit quadword (real
// amd64's own MOV r/m64,imm32 semantics) -- confirmed against real
// 6a/6l: "MOVQ $0xFFFFFFF6,AX" -> `b8 f6 ff ff ff` (zero-extends to
// 0x00000000FFFFFFF6) vs "MOVQ $0xFFFFFFF6,-8(SP)" -> `48 c7 44 24 f8
// f6 ff ff ff` (sign-extends to 0xFFFFFFFFFFFFFFF6, i.e. -10).
//
// Also exercises MOVL (same opcode family regardless of range, ymovl
// has no Ys32/Yi32 split at all) and MOVW (truncates any value,
// however large, to its own low 16 bits).
TEXT _start(SB), $0
	MOVQ	$0xFFFFFFF6, AX  // Yi32-but-not-Ys32: Ziq_rp's no-REX downgrade,
	                        // zero-extends -> AX = 0x00000000FFFFFFF6
	MOVQ	$-10, BX        // BX = 0xFFFFFFFFFFFFFFF6 (sign-extended -10)
	CMPQ	AX, BX          // different 64-bit values despite the same
	                        // low-32-bit pattern -- see comment above
	JNE	different
	MOVQ	$1, DI
	MOVQ	$60, AX
	SYSCALL
different:
	MOVQ	$0xFFFFFFF6, -8(SP)   // memory destination -- sign-extends
	MOVQ	-8(SP), CX
	CMPQ	CX, BX                // CX should now equal BX (both -10)
	JEQ	memok
	MOVQ	$2, DI
	MOVQ	$60, AX
	SYSCALL
memok:
	MOVL	$0xFFFFFFF6, DX       // L-width: same opcode family regardless
	MOVW	$0x12345678, SI       // W-width: truncates to the low 16 bits
	CMPW	SI, $0x5678
	JEQ	ok
	MOVQ	$3, DI
	MOVQ	$60, AX
	SYSCALL
ok:
	MOVQ	$77, DI               // exit(77)
	MOVQ	$60, AX
	SYSCALL
