// Exercises the 8-bit (B-suffixed) forms: MOVB immediate-to-register
// (goken's Zib_rp, "b0+reg"+imm8, no ModRM) and immediate-to-memory
// (Zibo_m/0xc6, also 1-byte immediate), MOVB/ADDB/CMPB register forms,
// and two real B_-specific quirks confirmed against goken's real 6a/6l
// (see Codegen6.ml's own comments):
//
//  - a destination/source of exactly AX (not R8, whose low 3 bits are
//    also 0 but which has no such encoding at all) takes goken's own
//    "op AL,imm8" special case (opcode alone, no ModRM at all -- e.g.
//    ADDB $3,AX -> "04 03", CMPB AX,$5 -> "3c 05") ahead of the general
//    0x80/ModRM immediate-group form any other register uses;
//  - SI/DI (register indices 6/7), used as a byte *register value* (not
//    as a memory addressing base -- SP as -8(SP) below needs no REX at
//    all), force an otherwise-empty REX byte to select the low byte
//    (SIL/DIL) instead of the legacy high-byte AH/CH/DH/BH encoding
//    real x86 would otherwise mean by the same ModRM field value.
//
// (SP itself can't be exercised as a byte *register* at all -- a real,
// if obscure, goken gap: oclass()'s own D_SPB case is commented out in
// span.c, confirmed empirically: "MOVB $5,SP" assembles fine under 6a
// but 6l's own doasm() fails to find a matching row for it at link
// time ("doasm: notfound ... MOVB $5,SPB") -- so there is no goken
// reference output to match here, and this port doesn't need to
// replicate the gap since nothing exercises it.)
//
// Self-check: 100 + 23 = 123 (AX, via the AL-special-case ADDB/CMPB),
// stored to and reloaded from memory, then combined with an SI/DI
// computation (9+3=12) for a final exit code of 135 -- same shape and
// expected result as movl_arith.s/movw_arith.s, at the narrowest width.
TEXT _start(SB), $0
	MOVB	$100, AX
	ADDB	$23, AX         // AX = 123 (Zib_ AL-special-case, "04 17")
	CMPB	AX, $123        // Z_ib AL-special-case, "3c 7b"
	JEQ	b_ok
	MOVB	$99, AX
b_ok:

	// claude: -8(SP)/-4(SP) red-zone scratch, same reasoning as
	// movl_arith.s's own comment.
	MOVB	AX, -8(SP)      // exercise MOVB reg->mem (Zr_m, 0x88)
	MOVB	$0, AX          // clobber AX -- MOVB has no Zclr row at
	                        // all (unlike MOVQ/MOVL/MOVW), so this is
	                        // an ordinary Zib_rp immediate move
	MOVB	-8(SP), AX      // exercise MOVB mem->reg (Zm_r, 0x8a) -- AX = 123
	MOVB	$77, -4(SP)     // exercise MOVB imm->mem (Zibo_m, 0xc6)
	CMPB	-4(SP), $77
	JEQ	mem_ok
	MOVB	$99, AX
mem_ok:

	MOVB	$9, SI          // both force an otherwise-empty REX byte
	MOVB	$3, DI          // (SIL/DIL, not AH/BH -- see prelude)
	ADDB	SI, DI          // DI = 12 ("40 00 f7", confirmed against
	                        // real 6a/6l)
	CMPB	DI, $12
	JEQ	r_ok
	MOVB	$99, AX
r_ok:

	ADDL	DI, AX          // AX = 123 + 12 = 135 (widen via a 32-bit
	                        // add, same reasoning as movw_arith.s's
	                        // own comment)
	MOVQ	AX, DI          // exit(135)
	MOVQ	$60, AX
	SYSCALL
