// "MOVx.P Rt,off(Rbase)" post-indexed-writeback addressing (ARM's real
// post-increment store/load), as used by goken's real 5c -S output
// for lib_core/libc/port/memset.c's byte-fill loop (and, the same
// idiom, memmove/memchr/strchr/strncpy/...) -- confirmed for real via
// docs/claude_notes/plan_hello_libc_linking.md's hello_libc stress
// test: o5a's grammar rejected "MOVB.P R6,1(R5)" outright (its TMOV
// production used the plain `cond` nonterminal, not `condf`, even
// though the lexer already tokenized ".P"/".W" as TSUF for MOVM) --
// see Ast_asm5.move_opt_of_flags's own comment for the fix.
//
// The caller (_start) and memset's own body below are both taken
// verbatim from goken's real `5c -S` output (only "main" -> "_start"
// and the exits() call -> a raw exit syscall, so this fixture doesn't
// need the rest of libc just to check this one construct); see that
// investigation for the exact `5c` invocation used to produce them.
// Fills an 8-byte stack buffer with 'x' (120) and exits with buf[0],
// so a working MOVB.P is checked functionally (under qemu-arm), not
// just for a clean parse.
//
// _check (not byte-identical, per tests/linker/README.md): after the
// MOVB.P fix above and a second real gap found alongside it (ADD/SUB
// with a negative immediate needs the same unconditional flip goken's
// linkers/5l/obj.c's ldobj() does -- "ADD $-1,R3,R3" -> "SUB
// $1,R3,R3" -- now also fixed, see Rewrite5.ml), the one remaining
// byte difference is goken eliminating the 3-instruction "B 3(PC)/B
// 2(PC)/B 6(PC)" dead-branch chain this loop's -S text uses to
// implement a do-while-style "check condition first" head, which o5l
// doesn't (yet) replicate -- a genuinely different, deeper class of
// gap (dead-code/branch-target-folding in the linker's noop pass),
// not attempted here. Confirmed functionally identical (exit code
// 120 both sides) despite the extra unreached instructions.

TEXT _start(SB), $20
	MOVW	$buf-8(SP),R0
	MOVW	$120,R1
	MOVW	R1,8(R13)
	MOVW	$8,R3
	MOVW	R3,12(R13)
	BL	memset+0(SB)
	MOVBU	0(R0),R1
	MOVW	R1,R0
	MOVW	$1,R7
	SWI	$0

TEXT memset(SB), $4
	MOVW	c+4(FP),R6
	MOVW	n+8(FP),R3
	MOVW	R0,R7
	MOVW	R0,R5
	B	3(PC)
	B	2(PC)
	B	6(PC)
	CMP	$0,R3
	BLS	-2(PC)
	MOVB.P	R6,1(R5)
	ADD	$-1,R3,R3
	B	-6(PC)
	MOVW	R7,R0
	RET
	RET
