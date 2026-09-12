// Same round trip as float_mem_case50_51.s, but VFP encoding (-f)
// instead of FPA -- exercises gfsr_vfp instead of gfsr.
TEXT _start(SB), $16
    MOVW    $42, R0
    MOVWF   R0, F0
    MOVF    F0, 0(R13)
    MOVF    0(R13), F1
    MOVFW   F1, R1
    MOVW    R1, R0
    MOVW    $1, R7
    SWI     $0
