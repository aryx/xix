// A shifted-register as a generic operand to *any* arithmetic
// instruction (not just the standalone SLL/SRL/SRA mnemonics) -- e.g.
// real 5c -S output for fmt/dofmt.c's "ADD R5->2,R2" or fmt/fmt.c's
// "ADD R5<<3,R3,R8". Confirmed real, valid 5a syntax (goken's real 5a
// accepts all three forms below): goken's own real 5a
// (assemblers/5a/a.y) doesn't lex "<<"/">>"/"->" as single tokens
// either, it combines two adjacent raw '<'/'>'/'-' tokens directly at
// the grammar level -- same fix here (Parser_asm5.mly's `shift` rule),
// not a new lexer token. See
// docs/claude_notes/plan_hello_libc_linking.md.

TEXT _start(SB), $0
	MOVW	$3,R3
	MOVW	$5,R5
	ADD	R5<<3,R3,R8	// R8 = 3 + (5<<3) = 43

	MOVW	$40,R2
	ADD	R2->2,R2	// R2 = 40 + (40 asr 2) = 50

	MOVW	$0,R1
	MOVW	$1,R5
	MOVW	$2,R6
	ORR	R5>>R6,R1,R1	// R1 = 0 | (1>>2 logical) = 0

	MOVW	R8,R0
	SUB	R2,R0,R0	// 43-50 = -7
	RSB	$0,R0,R0	// 7
	ADD	R1,R0,R0	// 7+0 = 7

	MOVW	R0,R7
	MOVW	$1,R7
	SWI	$0
