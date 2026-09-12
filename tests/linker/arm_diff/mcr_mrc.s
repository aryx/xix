// MCR/MRC (coprocessor register move). Unlike every other case,
// goken computes the encoded word directly in its grammar action
// (a.y's own comment: "MCR MRC"), not via codegen.c -- so this
// reuses the plain WORD pseudo-op mechanism on the xix side too, see
// the MCR/MRC production in Parser_asm5.mly.
//
// Coprocessor access is normally privileged, but this specific read
// (CP15's Main ID Register, c0,c0,0) is one qemu-arm's user-mode
// emulation actually implements rather than trapping -- so unlike
// fcr_move_case56_57.s's FPCR aside or psr_move_case35_36_37.s's
// SPSR aside, this is a genuine functional round-trip: the exit code
// is MIDR's low byte, identical on both sides.
//
// MCR (write side) was also verified ad hoc, same pattern as those
// FPCR/SPSR asides: "MCR 15,0,R0,C1,C0,0" (writing SCTLR) is
// byte-identical against goken and traps identically as "Illegal
// instruction" on both sides -- not added as its own fixture since
// it adds no further coverage beyond confirming the shared encoding.
TEXT _start(SB), $0
    MRC 15, 0, R0, C0, C0, 0
    MOVW $1, R7
    SWI $0
