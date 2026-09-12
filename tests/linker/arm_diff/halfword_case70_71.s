// Halfword short-offset store (case 70, STRH) and signed byte /
// halfword short-offset load (case 71, LDRSB/LDRSH): store a
// halfword, then read it back both as a halfword and as a signed
// byte.
TEXT _start(SB), $16
    MOVW    $0x1234, R1
    MOVH    R1, 4(R13)
    MOVH    4(R13), R2
    MOVB    4(R13), R3
    MOVW    R2, R0
    MOVW    $1, R7
    SWI     $0
