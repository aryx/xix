// Floating point store (case 50) and load (case 51), FPA (the
// default -- goken's own `vfp = debug['f']` flag off): MOVF F0,
// offset(R13) then MOVF offset(R13),F1 through a plain
// register+offset address, mirroring case 20/21's int store/load
// (base_and_offset_of_indirect is reused as-is -- the memory side of
// a float move still addresses through an ordinary integer base
// register, only the data register is a float one). Round-trips 42
// through memory and back via MOVWF/MOVFW (case 55) to land in the
// exit code, so a wrong offset or wrong FPA bits would show up as a
// wrong exit code, not just a byte diff.
TEXT _start(SB), $16
    MOVW    $42, R0
    MOVWF   R0, F0
    MOVF    F0, 0(R13)
    MOVF    0(R13), F1
    MOVFW   F1, R1
    MOVW    R1, R0
    MOVW    $1, R7
    SWI     $0
