// Big-constant arithmetic: ADD $0x12345678, R0, R1 doesn't fit ARM's
// 8-bit rotated immediate encoding, so goken must fall back to
// loading the constant into REGTMP via the literal pool first (case
// 13: op $lcon, [R], R) instead of a single ADD instruction.
TEXT _start(SB), $0
    MOVW    $10, R0
    ADD     $0x12345678, R0, R1
    MOVW    $1, R7
    SWI     $0
