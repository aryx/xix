// Functional check for case 34: load 42 into F0 via MOVW $42,F0
// then read it back via MFC1 (case 31) and self-check with BEQ.
// Not byte-identical: the MFC1 read immediately after the float-
// constant load needs its own mandatory delay slot (case 31's nop,
// see case30_31_mips.s), and this also inherits BEQ/JMP's own
// delay-slot gap (see case6_mips.s).
TEXT _start(SB), $0
    MOVW    $0, R4
    MOVW    $42, F0
    MOVW    F0, R1
    MOVW    $42, R3
    BEQ     R1, R3, ok1
    JMP     after1
ok1:
    ADD     $1, R4, R4
after1:
    MOVW    $4001, R2
    SYSCALL
