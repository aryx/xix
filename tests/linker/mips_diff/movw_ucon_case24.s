// MOVW $con,r (case 24) at the UCON boundary: low 16 bits all
// zero, magnitude beyond case 3's ANDCON range -- a single LUI, no
// OR needed. Covers both a positive (0x10000) and a negative
// (-0x10000) UCON value (confirmed against goken directly via
// `vl -a`: both single-instruction, no literal-pool rewriting --
// unlike a *non*-UCON large literal, see the comment on case 19's
// removed plain-literal arm in Codegenv.ml).
TEXT _start(SB), $0
    MOVW    $65536, R1
    MOVW    $-65536, R3
    MOVW    $0, R4
    MOVW    $4001, R2
    SYSCALL
