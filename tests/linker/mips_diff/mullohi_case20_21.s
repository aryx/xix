// MOVW r,LO/HI (case 21, MTLO/MTHI) and MOVW LO/HI,r (case 20,
// MFLO/MFHI), entirely independent of MUL (case 22) -- writing then
// reading LO/HI directly is fully byte-identical (confirmed against
// goken directly via `vl -a`). Deliberately does NOT go through MUL
// here: goken's sched.c pads two NOPs around certain MUL-result
// read/write transitions on the HI/LO pseudo-registers (a real
// hazard-avoidance heuristic, empirically confirmed but not fully
// characterized) -- the same already-documented, deliberately
// unported scheduler gap as every branch/call delay slot this
// session. See mullohi_case20_22_check.s for a MUL-based functional
// check that accepts that diff.
TEXT _start(SB), $0
    MOVW    $6, R1
    MOVW    $7, R2
    MOVW    R1, LO
    MOVW    R2, HI
    MOVW    LO, R4
    MOVW    HI, R5
    SUB     R4, R5, R6
    MOVW    $4001, R2
    SYSCALL
