// Same as float_mem_longoff_case52_53.s, but VFP encoding (-f).
TEXT _start(SB), $8200
    MOVW    $42, R0
    MOVWF   R0, F0
    MOVF    F0, 4096(R13)
    MOVF    8192(R13), F1
    MOVFW   F1, R1
    MOVW    R1, R0
    MOVW    $1, R7
    SWI     $0
