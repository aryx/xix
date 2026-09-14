// Exercises ANDW/ORRW/EORW's own 32-bit-view bitmask-immediate form
// (case 53, N always 0, e<=32) -- unlike the bare 64-bit AND/ORR/EOR
// immediate (see bitmask_logical.s), this port never tries to
// replicate goken's own real narrow-element-size encoding here at
// all: direct testing against real goken already turned up a
// confirmed bug in its bitmask-immediate assembler for some
// sub-64-bit element sizes (see Codegen7.ml's isbitcon/
// bitcon64_params comment), and the W-suffixed forms inherently need
// e<=32 for every single value, not just some. Instead this port
// always takes a genuinely different, xix-only path: materialize the
// immediate into REGTMP, then use the plain register-register AND/
// ORR/EOR form (real hardware, byte-parity-safe on its own) -- see
// Codegen7.ml's own comment on the ANDW/ORRW/EORW Arith case. NOT
// expected to be byte-identical to goken (2 real instructions here
// vs goken's 1), only functionally identical -- same category as the
// MOVT-store-immediate expansion documented in sized_move.s's own
// closure notes.
//
// R1=200=0b11001000. ANDW $15 -> 8. ORRW $16 -> 24. EORW $7 -> 31.
TEXT _start(SB), $0
    MOV $200, R1
    ANDW $15, R1, R2
    ORRW $16, R2, R2
    EORW $7, R2, R0
    MOV $93, R8
    SVC $0
