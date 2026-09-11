// MUL (case 22), byte-comparison only -- no HI/LO readback here on
// purpose (see mullohi_mips.s/mullohi_check_mips.s for why: reading
// a MUL's result triggers a scheduler-dependent NOP-padding gap
// this port doesn't replicate). Confirms MUL R1,R2's own encoding
// in isolation.
TEXT _start(SB), $0
    MOVW    $6, R1
    MOVW    $7, R2
    MUL     R1, R2
    MOVW    $3, R4
    MOVW    $4001, R2
    SYSCALL
