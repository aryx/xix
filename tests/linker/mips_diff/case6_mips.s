// Conditional branches (case 6): BEQ/BNE (2-register form) plus two
// of the Bxx family (single register vs zero) -- BGTZ and BLTZ.
// Each branch takes its "taken" path and accumulates into the
// exit-code register (R4); the "not taken" path (unreachable here,
// but still assembled/encoded) sets an out-of-range wrong value so a
// bug in the branch encoding or its delay slot would be caught by
// qemu-mips, not just by the byte comparison.
TEXT _start(SB), $0
    MOVW    $5, R1
    MOVW    $5, R2
    MOVW    $7, R3
    MOVW    $0, R4

    BEQ     R1, R2, eq_taken
    MOVW    $100, R4
    JMP     after_eq
eq_taken:
    ADD     $1, R4, R4
after_eq:

    BNE     R1, R3, ne_taken
    MOVW    $200, R4
    JMP     after_ne
ne_taken:
    ADD     $2, R4, R4
after_ne:

    MOVW    $5, R5
    BGTZ    R5, gtz_taken
    MOVW    $300, R4
    JMP     after_gtz
gtz_taken:
    ADD     $4, R4, R4
after_gtz:

    MOVW    $-3, R6
    BLTZ    R6, ltz_taken
    MOVW    $400, R4
    JMP     after_ltz
ltz_taken:
    ADD     $8, R4, R4
after_ltz:

    MOVW    $4001, R2
    SYSCALL
