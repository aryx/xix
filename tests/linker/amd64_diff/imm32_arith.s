// Exercises the general (non-AX-destination) Zilo_m/Zm_ilo (opcode
// 0x81) imm32 arith/cmp forms -- andorshift.s only exercises the
// AX-implicit Zil_/Z_il special case (opcode (ext<<3)|0x05, no
// ModRM); this fixture exercises the general ModRM+imm32 form any
// other destination register falls through to (confirmed against
// real 6a/6l: "SUBQ $900,BX" -> "48 81 eb 84 03 00 00").
//
// Self-check: (0x1000 - 900) = 3196, then OR/AND-masked to 11388,
// each step verified via CMPQ/CMPL's own imm32 form (also the general
// ModRM one, not the AX-special) before continuing, for a final exit
// code of 88.
TEXT _start(SB), $0
	MOVQ	$0x1000, BX
	SUBQ	$900, BX        // BX = 4096-900 = 3196 (general Zilo_m form)
	CMPQ	BX, $3196       // general Zm_ilo form
	JEQ	ok1
	MOVQ	$1, AX
	MOVQ	$60, AX
	SYSCALL
ok1:
	ORL	$0x2000, BX     // BX = 3196 | 8192 = 11388
	ANDL	$0x3FFF, BX     // no-op mask, still 11388
	CMPL	BX, $11388
	JEQ	ok2
	MOVQ	$1, AX
	MOVQ	$60, AX
	SYSCALL
ok2:
	MOVQ	$88, DI         // exit(88)
	MOVQ	$60, AX
	SYSCALL
