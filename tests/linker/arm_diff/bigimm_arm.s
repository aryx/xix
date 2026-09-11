// Big-constant MOVW: 0x12345678 doesn't fit ARM's 8-bit rotated
// immediate encoding, so goken must fall back to a literal-pool load
// (case 12: movw $lcon, reg) instead of a single MOV instruction.
TEXT _start(SB), $0
    MOVW    $0x12345678, R0
    MOVW    $1, R7
    SWI     $0
