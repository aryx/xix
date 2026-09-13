// Exercises the 16-bit (W-suffixed) forms: MOVW immediate-to-register
// (goken's Zil_rp, same "op+reg, no ModRM" shape as MOVL's own, just a
// 2-byte immediate) and immediate-to-memory (Zilo_m/0xc7, also 2-byte
// immediate), MOVW/ADDW/CMPW register forms. All of these reuse the
// exact same opcodes as their L-suffixed counterparts (goken's own
// yaddl/yxorl/ycmpl/ymovw tables are shared across L and W), the only
// real difference being the mandatory 0x66 "Pe" operand-size-override
// prefix -- confirmed to come *before* any REX byte (see Codegen6.ml's
// own `prefix66` comment) -- and R8-R15 still needing REX.B/.R exactly
// as for every other width (Q_/L_/W_ share the same rex_opt/
// rex_b_of_resolved_gen logic, only the REX.W bit itself differs).
//
// Self-check: 100 + 23 = 123 (register form), stored to and reloaded
// from memory (exercising MOVW's own reg<->mem forms), then combined
// with an R8/R9 computation (9+3=12) for a final exit code of 135 --
// same shape and same expected result as movl_arith.s, just at the
// narrower width, to make the two easy to compare against each other.
TEXT _start(SB), $0
	MOVW	$100, AX
	MOVW	$23, BX
	ADDW	BX, AX          // AX = 123
	CMPW	AX, $123
	JEQ	w_ok
	MOVW	$999, AX
w_ok:

	// claude: -8(SP)/-4(SP) red-zone scratch, same reasoning as
	// movl_arith.s's own comment (SP is a real, unadjusted register on
	// this arch, see Ast_asm6.ml's prelude).
	MOVW	AX, -8(SP)      // exercise MOVW reg->mem (Zr_m)
	MOVW	$0, AX          // clobber AX -- exercises MOVW's own Zclr
	                        // row too (same Yi0 row as MOVQ/MOVL)
	MOVW	-8(SP), AX      // exercise MOVW mem->reg (Zm_r) -- AX = 123
	MOVW	$77, -4(SP)     // exercise MOVW imm->mem (Zilo_m)
	CMPW	-4(SP), $77
	JEQ	mem_ok
	MOVW	$999, AX
mem_ok:

	MOVW	$9, R8
	MOVW	$3, R9
	ADDW	R8, R9          // R9 = 12
	CMPW	R9, $12
	JEQ	r_ok
	MOVW	$999, AX
r_ok:

	ADDL	R9, AX          // AX = 123 + 12 = 135 (widen via a 32-bit
	                        // add -- MOVW's own 16-bit view doesn't
	                        // zero-extend to 64 like a 32-bit write
	                        // does, so this avoids stale upper bits)
	MOVQ	AX, DI          // exit(135)
	MOVQ	$60, AX
	SYSCALL
