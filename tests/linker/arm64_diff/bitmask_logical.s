// Exercises AND/ORR/EOR's own "bitmask immediate" form (case 53),
// scoped to e=64 patterns only (a single contiguous run of 1-bits,
// any rotation, never a smaller replicated element) -- see
// Codegen7.ml's bitmask_immediate_encoding comment for why: direct
// testing against real goken turned up a genuine bug in its own
// bitmask-immediate assembler for sub-64-bit element sizes (a clean
// e=8 pattern like 0x0202020202020202 assembles to bytes that decode
// back to a different value entirely), so this port deliberately
// doesn't try to replicate that uncertain behavior.
//
// R1=200=0b11001000. AND $15 -> 8. ORR $16 -> 24. EOR $7 -> 31
// (verified not just byte-identical against goken but numerically
// correct too). AND $0x1FE (a rotated e=64 pattern, 8 ones starting
// at bit 1) exercises the nonzero-rotation immr encoding.
TEXT _start(SB), $0
    MOV $200, R1
    AND $15, R1, R2
    ORR $16, R2, R2
    EOR $7, R2, R0
    MOV $511, R3
    AND $0x1FE, R3, R4
    MOV $93, R8
    SVC $0
