// Register-register arith (case 0: ADD/SUB/AND/OR/XOR/SLT/SLTU/SLL/
// SRL/SRA) and shift-by-immediate (case 1: SLL/SRL/SRA), computing
// directly into the exit-code register. Linux RISC-V syscall ABI:
// a0=R10 (arg/exit code), a7=R17 (syscall number); exit=93.
TEXT _start(SB), $0
    MOVW    $10, R1
    MOVW    $3, R2
    ADD     R1, R2, R3      // R3 = 10+3 = 13
    SUB     R3, R1, R4      // R4 = 13-10 = 3
    SLL     $2, R4, R5      // R5 = 3<<2 = 12
    SRL     $1, R5, R6      // R6 = 12>>1 = 6
    SRA     $1, R6, R7      // R7 = 6>>1 = 3 (arithmetic, same as logical here)
    AND     R6, R2, R8      // R8 = 6 & 3 = 2
    OR      R8, R1, R9      // R9 = 2 | 10 = 10
    XOR     R9, R7, R10     // R10 = 10 ^ 3 = 9
    SLT     R1, R2, R11     // R11 = (10 < 3) = 0
    SLTU    R2, R1, R12     // R12 = (3 < 10) = 1
    ADD     R10, R12, R10   // R10 = 9 + 1 = 10
    ADD     R10, R11, R10   // R10 = 10 + 0 = 10 (exit code)
    MOVW    $93, R17        // a7 = syscall number: exit
    ECALL
