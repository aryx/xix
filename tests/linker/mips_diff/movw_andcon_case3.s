// MOVW $con,r (case 3) at the ANDCON boundary (0x8000-0xffff):
// goken uses ORI here, not ADDIU -- ADDIU would sign-extend the
// immediate and produce the wrong value (e.g. $0x8000 would become
// -32768 instead of +32768). No branches needed here (unlike
// immcon_case4_10.s's case 4/10 fixture), so this one is directly
// byte-comparable.
TEXT _start(SB), $0
    MOVW    $32768, R1
    MOVW    $65535, R2
    MOVW    $0, R4
    MOVW    $4001, R2
    SYSCALL
