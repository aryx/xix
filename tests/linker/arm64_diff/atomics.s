// Exercises the X-width exclusive-monitor atomic pair (case 58/59):
// LDXR/STXR (plain) and LDAXR/STLXR (acquire/release). goken's own
// grammar order for STXR/STLXR ("Rt(value),(Rn)(address),Rs(status)")
// was confirmed empirically against real goken -- not assumed from
// the C source alone, given this family's documented bug history --
// see Ast_asm7.ml's StoreExcl comment.
TEXT _start(SB), $0
    MOV $setSB(SB), R28
    MOV $10, R1
    MOV $foo(SB), R2
    STXR R1, (R2), R3
    LDXR (R2), R4
    MOV $20, R5
    STLXR R5, (R2), R6
    LDAXR (R2), R7
    ADD R4, R7, R0
    MOV $93, R8
    SVC $0
DATA foo+0(SB)/8, $0
GLOBL foo(SB), $8
