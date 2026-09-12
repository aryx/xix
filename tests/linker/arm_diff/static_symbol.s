// Exercises Plan 9's "foo<>" static/local-symbol syntax (goken's own
// arch/arm/div.s uses it for its software divide helpers: "save<>",
// "rest<>", "div<>"). This was never wired at all before: the shared
// Lexer_asm.mll never tokenized '<'/'>' (see Token_asm.ml's TLT/TGT
// comment), even though every arch's Parser_asmX.mly already had a
// "TIDENT TLT TGT offset TOPAR TSB TCPAR" grammar rule for it, and the
// linker (Types.ml's Private/Load.ml's per-object-file id) already
// fully supported the resulting AST -- so any real 5c-generated
// assembly (which prints string/float literal pools as "foo<>+N(SB)")
// failed to assemble at all before this fix.
//
// A "<>" symbol is local to its own object file -- each o5a
// invocation gets its own Private id (Load.ml), so two same-named
// "<>" symbols from two different .s files (or two o5a runs) don't
// collide, only within one file if reused, as here.
TEXT _start(SB), $0
	MOVW	$val<>+0(SB), R1
	MOVW	0(R1), R0
	MOVW	$1, R7
	SWI	$0

DATA	val<>+0(SB)/4, $42
GLOBL	val<>+0(SB), $4
