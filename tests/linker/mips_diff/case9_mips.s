// Shift-by-register (case 9): SLL/SRL/SRA with a register shift
// amount (not an immediate -- that's case 16, separate).
TEXT _start(SB), $0
    MOVW    $1, R1
    MOVW    $4, R5
    SLL     R5, R1, R6      // R6 = R1 << R5 = 16
    MOVW    $2, R7
    SRL     R7, R6, R4      // exit code = R6 >> R7 = 4
    MOVW    $4001, R2
    SYSCALL
