// Functional check for case 22 (MUL): 6*7=42, read back via MOVW
// LO,r (case 20) right after the MUL and self-checked with BEQ. Not
// byte-identical: this fixture inherits *two* known,
// already-documented scheduler gaps -- goken's sched.c pads NOPs
// around certain MUL-result HI/LO read/write transitions (see
// mullohi_mips.s), and BEQ/JMP's own delay-slot scheduling (see
// case6_mips.s) -- see docs/claude_notes/todo_mips_port.org.
TEXT _start(SB), $0
    MOVW    $0, R4
    MOVW    $6, R1
    MOVW    $7, R2
    MUL     R1, R2
    MOVW    LO, R3
    MOVW    $42, R5
    BEQ     R3, R5, ok1
    JMP     after1
ok1:
    ADD     $1, R4, R4
after1:
    MOVW    $4001, R2
    SYSCALL
