// mov r,mr (case 37, MTC0/DMTC0), coprocessor-0/MMU register write.
// No delay-slot hazard for the write direction -- byte-identical.
// See cop0_move_case37_38_check.s for a functional round-trip check of
// case 38 (MFC0/DMFC0, the read direction), which does have a
// 2-NOP delay-slot hazard (goken's noop.c has a dedicated always-2
// -NOP special case for any AMOVW/AMOVV whose source is D_MREG,
// distinct from the 1-NOP COP1/load hazards elsewhere this
// session -- see Codegenv.ml's case 38 comment).
TEXT _start(SB), $0
    MOVW    $42, R1
    MOVW    R1, M5
    MOVV    R1, M6
    MOVW    $0, R4
    MOVW    $4001, R2
    SYSCALL
