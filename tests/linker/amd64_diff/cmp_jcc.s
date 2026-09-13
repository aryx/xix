// Exercises CMPQ + the four condition codes wired so far (JEQ/JNE/
// JLT/JGE), via the short (rel8) form -- see Ast_asm6.ml's `condition`
// comment and Codegen6.ml's own Jcc comment for why only the short
// form is implemented (goken's own real relaxation between short/near
// forms is a genuine multi-pass sizing problem).
//
// JMP is deliberately not exercised here: goken's own linker turned
// out to apply real, non-trivial control-flow transformations that
// are out of scope to replicate for this checkpoint -- confirmed
// empirically while first trying to test it: a "JMP L; <anything>; L:"
// forward skip gets deleted (goken removes code reachable only via
// falling through a since-optimized-out unconditional jump, plus the
// jump itself), and a backward "loop: ...; JMP loop" gets *loop-
// rotated* (goken duplicates the loop's leading compare-and-exit into
// a trailing conditional branch, avoiding the unconditional jump
// entirely) -- genuine compiler-style peephole optimizations in
// goken's own pipeline (plausible given goken's own Go-toolchain
// lineage), not simple assembler passthrough. JMP's own AST/grammar/
// Codegen6.ml wiring is still real and believed correct (it's goken's
// side that transforms the surrounding code, not this port's own
// encoding that's wrong) -- just not yet covered by a fixture that
// avoids triggering either transformation. See plan_amd64_port.md.
//
// Deliberately avoids "MOVQ $0,R" anywhere: goken's own ymovq table
// matches an immediate of exactly 0 to its own dedicated Yi0/Zclr row
// (a 3-byte XOR-based encoding) *before* ever trying the general
// Ys32/Zilo_m row this port implements -- confirmed in optab.c. AX is
// zeroed with "XORQ AX,AX" instead (already exercised by
// hello_linux.s's own "XORQ DI,DI").
//
// All four checks below are written so the condition is *true* and
// the jump *is* taken (skipping its own "MOVQ $999,AX" failure
// marker) -- a self-check: if any jump were wrongly encoded (not
// taken, or taken to the wrong place), AX would end up 999 instead
// of 42. Confirmed these forward conditional skips are *not* subject
// to goken's dead-code elision (a conditional jump's own fallthrough
// is always statically reachable, unlike an unconditional one).
TEXT _start(SB), $0
	XORQ	AX, AX          // AX = 0

	MOVQ	$5, BX
	CMPQ	BX, $5
	JEQ	eq_ok           // 5 == 5, taken
	MOVQ	$999, AX
eq_ok:

	MOVQ	$3, BX
	CMPQ	BX, $5
	JNE	ne_ok           // 3 != 5, taken
	MOVQ	$999, AX
ne_ok:

	MOVQ	$3, BX
	CMPQ	BX, $5
	JLT	lt_ok           // 3 < 5, taken
	MOVQ	$999, AX
lt_ok:

	MOVQ	$7, BX
	CMPQ	BX, $5
	JGE	ge_ok           // 7 >= 5, taken
	MOVQ	$999, AX
ge_ok:

	ADDQ	$42, AX         // AX = 42

	MOVQ	AX, DI          // exit(42)
	MOVQ	$60, AX
	SYSCALL
