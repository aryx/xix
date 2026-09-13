// DIV/MOD/DIVU/MODU -- real ARM has no hardware divide, so goken's
// real linker expands these into a call sequence to a software helper
// in arch/arm/div.s. This port deliberately does NOT replicate that
// (div.s needs "NAME = value" + "R(name)" syntax this assembler
// doesn't support, a separate, larger, not-yet-attempted feature) --
// it uses real ARMv7 hardware SDIV/UDIV instructions instead,
// confirmed to work under qemu-arm. See Ast_asm5.DIVU's own comment
// and docs/claude_notes/plan_hello_libc_linking.md.
//
// _check (not byte-identical, per tests/linker/README.md): this is a
// deliberate, documented DEVIATION from goken's real bytes/behavior
// (a different division mechanism entirely), not just a syntax
// accommodation like CASE/BCASE elsewhere -- there is no meaningful
// byte comparison to make here. DIV/MOD ARE real 5a mnemonics (DIVU/
// MODU are not, same xix-only-syntax category as CASE/BCASE), so
// goken CAN assemble+link this fixture, but its behavior differs
// (goken calls _div/_divu/_mod/_modu, this port uses SDIV/UDIV/MLS
// directly) -- only xix's own result is checked here.
//
// 17/5=3, 17%5=2, 100%7=2 -- exit code 3+2+2=7.

TEXT _start(SB), $0
	MOVW	$17,R0
	MOVW	$5,R1
	DIV	R1,R0	// R0 = 17/5 = 3
	MOVW	R0,R8

	MOVW	$17,R0
	MOVW	$5,R1
	MOD	R1,R0	// R0 = 17%5 = 2
	ADD	R0,R8

	MOVW	$100,R0
	MOVW	$7,R1
	MODU	R1,R0,R5	// R5 = 100%7 = 2
	ADD	R5,R8

	MOVW	R8,R0
	MOVW	$1,R7
	SWI	$0
