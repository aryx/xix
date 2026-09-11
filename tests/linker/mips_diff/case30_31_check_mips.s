// Functional check for case 30/31 (MTC1/MFC1): round-trip 42
// through F0 and read it back, self-checked with BEQ. Not
// byte-identical: chaining MTC1 straight into an MFC1 of the same
// register makes goken's sched.c hoist a real instruction into the
// MTC1 delay slot (and specifically avoid a second COP1 transfer
// there) instead of this port's plain NOP -- see case30_31_mips.s's
// comment, and Codegenv.ml's case 30/31 comment, for the byte-
// identical, isolated case instead. Also inherits BEQ/JMP's own
// delay-slot gap (see case6_mips.s).
TEXT _start(SB), $0
    MOVW    $0, R4
    MOVW    $42, R1
    MOVW    R1, F0
    MOVW    F0, R2
    MOVW    $42, R3
    BEQ     R2, R3, ok1
    JMP     after1
ok1:
    ADD     $1, R4, R4
after1:
    MOVW    $4001, R2
    SYSCALL
