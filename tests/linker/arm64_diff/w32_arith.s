// Exercises the *W-suffixed 32-bit-view forms of Arith/Shift/Cmp/
// ArithMul (ADDW/SUBW/MULW/LSLW/CMPW/ANDW and friends) -- goken's own
// encoding for every one of these is identical to its bare 64-bit
// counterpart with only the "sf" bit (bit 31) cleared, confirmed
// directly against asmout.c's oprrr()/opirr() tables (see
// Codegen7.ml's oprrr_arith/opirr_addsub/oprrr_cmp/opirr_cmp/
// oprrr_shift/oprrr_mul comments). Register setup uses bare (64-bit)
// MOV since "MOVW $con,R" -- the W-sized register-immediate move --
// isn't implemented yet (a separate, still-deferred piece of work,
// see arm64_port.md).
TEXT _start(SB), $0
    MOV $10, R1
    MOV $11, R2
    ADDW R1, R2, R0   // R0 = 21
    SUBW R1, R0, R0   // R0 = 11
    MULW R2, R0, R0   // R0 = 121
    LSLW $1, R0, R0   // R0 = 242
    CMPW $200, R0     // 242 != 200, BEQ not taken
    BEQ skip
    MOV $1, R0
skip:
    ANDW R2, R0, R0   // R0 = 1 & 11 = 1
    MOV $93, R8
    SVC $0
