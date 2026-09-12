// FPA (case 55) int<->float conversion round trip: 42 -> F0 -> R1 ->
// exit(42). PASS byte-identical, AND (unlike fpa_case54.s's plain
// arithmetic) genuinely runs correctly under qemu-arm -- rc=42 both
// sides, so qemu apparently does handle these two specific FPA
// opcodes even though it doesn't handle FPA arithmetic.
TEXT _start(SB), $0
    MOVW    $42, R0
    MOVWF   R0, F0
    MOVFW   F0, R1
    MOVW    R1, R0
    MOVW    $1, R7
    SWI     $0
