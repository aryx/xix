// Registers in Plan 9 syntax correspond roughly as:
// R1 → $at
// R2 → $v0
// R3–R10 → $a0–$a7

TEXT _start+0(SB), $0

    MOVW $42, R4    // a0 = exit code
    MOVW $4001, R2  // v0 = syscall number (exit)
    SYSCALL
    RET              // not reached
