// Exercises FMOVD's other two shapes not covered by float_arith.s:
// float<->memory (both the plain register-base Indirect form and the
// SB-relative fast path, case 20/21 with the "V" bit set -- see
// Codegen7.ml's ldstr12u_size_v_opc comment) and float<->float
// register move (case 54's "monadic" branch, oprrr_fmovreg).
TEXT _start(SB), $0
    MOV $setSB(SB), R28
    MOV $100, R1
    SCVTFD R1, F1
    FMOVD F1, buf(SB)
    FMOVD buf(SB), F2
    FMOVD F2, F3
    FCVTZSD F3, R0
    MOV $93, R8
    SVC $0
DATA buf+0(SB)/8, $0
GLOBL buf(SB), $8
