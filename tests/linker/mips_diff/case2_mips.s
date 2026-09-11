// Generic register-register-register arith (case 2): add/sub/and,
// computing directly into the exit-code register to avoid needing
// a still-unported plain register-to-register MOVW.
TEXT _start(SB), $0
    MOVW    $10, R1
    MOVW    $3, R5
    ADD     R1, R5, R6
    SUB     R6, R1, R7
    AND     R7, R6, R4
    MOVW    $4001, R2
    SYSCALL
