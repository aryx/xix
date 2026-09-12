// Exercises int<->float conversion (SCVTFD/FCVTZSD, case 29) and
// dyadic double-precision float arith (FADDD/FSUBD/FMULD/FDIVD,
// case 54) -- see Codegen7.ml's oprrr_farith/oprrr_fcvt comments.
// goken's own float-immediate operand support is dead code in the
// reference implementation itself, so every float value here
// originates from an int->float conversion rather than a literal.
//
// R1=10, R2=3 (as doubles): F3 = (R2+R1)=13; F3 = R2*F3 = 39;
// F3 = F3-R1 = 29; F4 = F3/R2 = 29/3 = 9.666..., truncated to 9 by
// FCVTZSD -- verified not just byte-identical against goken but also
// numerically correct (exit code 9).
TEXT _start(SB), $0
    MOV $10, R1
    MOV $3, R2
    SCVTFD R1, F1
    SCVTFD R2, F2
    FADDD F2, F1, F3
    FMULD F2, F3, F3
    FSUBD F1, F3, F3
    FDIVD F2, F3, F4
    FCVTZSD F4, R0
    MOV $93, R8
    SVC $0
