// VFP (case 76, -f) int<->float conversion round trip: 42 -> F0 ->
// R1 -> exit(42). Two VMOV shuttle instructions plus the actual
// convert on each side (see gop_fixfloat_vfp_to_float/to_int in
// Codegen5.ml) -- genuinely runs correctly under qemu-arm, rc=42
// both sides.
TEXT _start(SB), $0
    MOVW    $42, R0
    MOVWF   R0, F0
    MOVFW   F0, R1
    MOVW    R1, R0
    MOVW    $1, R7
    SWI     $0
