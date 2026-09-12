// Functional check for case 27 (float load): store 42 into a float
// register, write it out via case 28 (ZOREG and LOREG), read it
// back via case 27, and self-check by round-tripping through MFC1
// (case 31) back to an int register compared with BEQ. Not
// byte-identical: case 27's mandatory load-delay-slot nop isn't
// always visible in goken's own output (sched.c hoists something
// else into the slot instead, same as every other scheduler gap
// this session), and this also inherits BEQ/JMP's own delay-slot
// gap and MFC1's own COP1-transfer delay-slot gap.
TEXT _start(SB), $0
    MOVW    $0, R4
    MOVW    $42, F0
    MOVF    F0, 0(R29)
    MOVF    F0, -8(R29)

    MOVF    0(R29), F1
    MOVW    F1, R1
    MOVW    $42, R2
    BEQ     R1, R2, ok1
    JMP     after1
ok1:
    ADD     $1, R4, R4
after1:

    MOVF    -8(R29), F2
    MOVW    F2, R1
    BEQ     R1, R2, ok2
    JMP     after2
ok2:
    ADD     $2, R4, R4
after2:

    MOVW    $4001, R2
    SYSCALL
