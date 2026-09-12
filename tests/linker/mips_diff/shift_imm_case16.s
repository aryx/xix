// Shift-by-immediate (case 16): SLL/SRL/SRA, computing directly
// into the exit-code register via chained shifts. Only the W-sized
// (32-bit) shift opcodes are exercised, matching case 9's own
// scoping (V-sized vlong shifts and the >=32 ALAST-aliased path
// aren't ported).
TEXT _start(SB), $0
    MOVW    $1, R1
    SLL     $4, R1, R2      // R2 = 1<<4 = 16
    SRL     $2, R2, R3      // R3 = 16>>2 = 4
    SRA     $1, R3, R4      // R4 = 4>>1 = 2
    MOVW    $4001, R2
    SYSCALL
