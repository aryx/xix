// VFP floating point (cases 74/75, -f): register-register arith
// starting from the zero-initialized register state (Linux's ELF
// loader zeroes all registers, VFP ones included, at process
// start), plus a compare to exercise case 75's 2-instruction form
// (VFP compare + MRS to move flags into CPSR for the branch).
// Unlike FPA (case 54), VFP is real, still-relevant hardware
// (qemu-arm actually emulates it) -- so unlike fpa_case54.s, this one
// should genuinely run, not just byte-compare.
TEXT _start(SB), $0
    ADDF    F0, F0, F1     // F1 = F0+F0 = 0.0 (F0 starts at 0.0)
    ADDF    F1, F1, F2     // F2 = F1+F1 = 0.0
    CMPF    F1, F2         // F1 == F2 (both 0.0)
    BEQ     eq

    MOVW    $1, R0
    MOVW    $1, R7
    SWI     $0

eq:
    MOVW    $0, R0
    MOVW    $1, R7
    SWI     $0
