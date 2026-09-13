// Exercises indirect CALL/JMP through a register -- goken's own
// ycall/yjmp "Zo_m64" row, opcode 0xff /2 (CALL) or /4 (JMP), reusing
// the shared A.branch_operand's existing IndirectJump constructor
// (already produced by this arch's own `branch: | ... | ireg { ref
// (IndirectJump $1) } ` grammar rule, copied from ARM64's template --
// no new AST case needed, see Codegen6.ml's own Call/Jmp comments).
//
// LEAQ is used to get a function's address into a register first
// (address-of-global, already wired) -- real Plan9 assembly can't
// take the address of a same-function local label this way (confirmed
// against real 6a: "LEAQ label(SB),R" for a same-TEXT label errors),
// so both indirect targets here are genuine separate TEXT globals.
//
// Self-check: retval() returns 7 via an indirect CALL; DI is set to
// that before an indirect JMP transfers control (no adjacent dead
// code, so goken's own unconditional-jump elision -- see cmp_jcc.s's
// header comment -- never enters the picture here) into exitnow,
// which exits with whatever DI already holds. Byte-identical against
// goken's real 6a/6l confirmed the encoding directly; exit code 7
// additionally confirms the CALL really returned into the right place
// (a wrong indirect target would either crash or never reach exitnow
// at all).
TEXT retval(SB), $0
	MOVQ	$7, AX
	RET

TEXT _start(SB), $0
	LEAQ	retval(SB), BX
	CALL	BX              // AX = 7
	MOVQ	AX, DI

	LEAQ	exitnow(SB), BX
	JMP	BX

TEXT exitnow(SB), $0
	MOVQ	$60, AX
	SYSCALL
