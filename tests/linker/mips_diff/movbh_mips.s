// MOVB/MOVH (case 12, sign-extend via SLL+SRA) and MOVBU/MOVHU
// (case 13, zero-extend via AND) between two registers, no memory
// involved. No branches needed, so this is directly
// byte-comparable (encoding confirmed against goken's `vl -a`
// output before writing this fixture). R1 = -1 (all bits set), so
// MOVBU/MOVHU should zero-extend to 255/65535 (SUB'ing them gives
// exactly 65280, i.e. exit code 0 mod 256) while MOVB/MOVH should
// sign-extend back to -1 (not separately checked here -- kept
// branch-free on purpose, see movbh_check_mips.s for a
// BEQ-verified functional check of the sign-extend side).
TEXT _start(SB), $0
    MOVW    $-1, R1
    MOVB    R1, R2
    MOVH    R1, R3
    MOVBU   R1, R5
    MOVHU   R1, R6
    SUB     R5, R6, R4
    MOVW    $4001, R2
    SYSCALL
