// Exercises Plan9's "foo<>" static/local-symbol syntax -- already
// wired at the shared Lexer_asm.mll/Parser_asm.mk_g level (see the
// ARM port's own commit that added TLT/TGT support across all archs)
// and copied into this arch's own grammar from the start, but never
// actually exercised by an amd64 fixture until now. `<>` symbols are
// scoped per object file (linker/Types.ml's Private/Load.ml), which
// only matters when linking multiple separately-assembled objects
// together -- a single-file fixture like this one can't exercise that
// disambiguation, only that the syntax parses and LEAQ resolves the
// symbol's address correctly (confirmed via the byte-identical
// comparison against goken's real 6a/6l, which pins down the exact
// resolved address, not just "does it parse").
//
// Doesn't dereference the resulting pointer (that would need
// arbitrary-register indirect addressing -- "MOVQ 0(BX),AX" -- which
// isn't wired yet, see plan_amd64_port.md's own "only SP as a memory
// base" scope note); LEAQ's own address-of-global resolution is what's
// under test here, matching how the .string<> pools that any real
// 6c-compiled string constant would produce could be resolved once
// this is wired.
TEXT _start(SB), $0
	LEAQ	val<>+0(SB), BX
	XORQ	DI, DI          // exit(0)
	MOVQ	$60, AX
	SYSCALL

DATA	val<>+0(SB)/8, $42
GLOBL	val<>(SB), $8
