// Minimal repro of the $sym(SB) operand kind mismatch: the principia
// lineage types the address of a symbol as D_ADDR (its 5.out.h merge
// design, matching what plan9 x86 8a always did), while the kencc 5a
// used plan9 arm's convention of D_CONST with a name. Since the two
// lineages now share include/5.out.h, the kencc lineage was taught
// D_ADDR: 5ak/a.y emits it for '$' oreg with a name, and 5lk reads it
// in zaddr()/aclass()/datblk() exactly like the old named D_CONST.
//
// MOVW $msg(SB) covers the instruction-operand path; DATA $msg(SB)
// covers the pointer-valued-data path in the linker's datblk().
TEXT _start(SB), $0
	MOVW	$msg(SB), R1
	MOVW	$1, R7
	SWI	$0

GLOBL	msg(SB), $4
DATA	msg+0(SB)/4, $"ok\n\z"

GLOBL	ptr(SB), $4
DATA	ptr+0(SB)/4, $msg(SB)
