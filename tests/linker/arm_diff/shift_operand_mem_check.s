// A scaled-register-offset memory address ("MOVB R5<<0(R7),R4" /
// "MOVB R4,R5<<0(R6)"), as used by real 5c -S output whenever it
// indexes a byte array with a loop variable (e.g. this copy1's own
// "dst[i] = src[i]"). See Ast_asm5.IndirectShift's own comment: this
// is NOT real 5a syntax either (confirmed against goken's real 5a,
// which has no source-level way to write register-offset memory
// addressing at all) -- a xix-only pipeline accommodation, same
// category as CASE/BCASE, .CC/.CS, and BL's "0(Rn)" elsewhere in this
// port. Only LSL-by-0 is implemented/verified (every real -S
// occurrence found so far uses it).
//
// The caller (_start) and copy1's own body below are both taken
// verbatim from goken's real `5c -S` output (only "main" -> "_start"
// and its trailing RET -> a raw exit syscall using the checksum
// already computed in R3), see
// docs/claude_notes/plan_hello_libc_linking.md for the exact `5c`
// invocations used to produce them.
//
// copy1 copies src[0..4] ("abcde") into dst, then _start sums
// dst[0..4]: 'a'+'b'+'c'+'d'+'e' = 495, exit code 495 mod 256 = 239.
//
// _check (not byte-identical, per tests/linker/README.md): goken's
// real 5a can't assemble this text at all (no source syntax for
// register-offset addressing), so there's no reference binary to
// byte-compare against -- verified instead by direct comparison
// against goken's copy1() compiled straight to a real object (5c, no
// -S text roundtrip) and linked with this same hand-written caller
// assembled by goken's own real 5a: the MOVB load/store instructions
// disassemble byte-identical, and both sides exit 239.

TEXT _start(SB), $28
	MOVW	$97,R1
	MOVB	R1,src-8(SP)
	MOVW	$98,R2
	MOVB	R2,src-7(SP)
	MOVW	$99,R3
	MOVB	R3,src-6(SP)
	MOVW	$100,R4
	MOVB	R4,src-5(SP)
	MOVW	$101,R5
	MOVB	R5,src-4(SP)
	MOVW	$dst-16(SP),R0
	MOVW	$src-8(SP),R1
	MOVW	R1,8(R13)
	BL	copy1+0(SB)
	MOVB	dst-12(SP),R3
	MOVB	dst-13(SP),R1
	ADD	R1,R3
	MOVB	dst-14(SP),R5
	ADD	R5,R3
	MOVB	dst-15(SP),R4
	ADD	R4,R3
	MOVB	dst-16(SP),R4
	ADD	R4,R3
	MOVW	R3,R0
	MOVW	$1,R7
	SWI	$0

TEXT copy1(SB), $4
	MOVW	src+4(FP),R7
	MOVW	R0,R6
	MOVW	$0,R5
	B	4(PC)
	B	2(PC)
	B	7(PC)
	ADD	$1,R5,R5
	CMP	$5,R5
	BGE	-3(PC)
	MOVB	R5<<0(R7),R4
	MOVB	R4,R5<<0(R6)
	B	-7(PC)
	MOVW	$0,R0
	RET
