// MIPS translation of goken's tests/s/variants/addr_arm.s (no MIPS
// equivalent exists upstream in goken -- hand-written). Exercises
// MOVW $sym(SB),R (address-of-global) and a pointer-valued DATA
// entry (GLOBL ptr(SB) holding the address of msg(SB), a relocation
// at data-write time, not just codegen).

TEXT _start(SB), $0
	MOVW	$setR30(SB), R30
	MOVW	$msg(SB), R5

	MOVW	$0, R4
	MOVW	$4001, R2
	SYSCALL

GLOBL	msg(SB), $4
DATA	msg+0(SB)/4, $"ok\n\z"

GLOBL	ptr(SB), $4
DATA	ptr+0(SB)/4, $msg(SB)
