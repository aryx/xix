// Exercises the conditional-select family (case 18: CSEL/CSET/CINC,
// goken's oprrr()-based encoding) and TBZ/TBNZ (case 40, test-bit-
// and-branch). CINC is the 2-register alias form of CSINC (see
// Ast_asm7.ml's CondSel comment for why they share one opcode
// constructor here, and Codegen7.ml's oprrr_condsel for the encoding
// and cond-inversion this implies).
//
// Uses the established "single skip, no trailing jump" shape (see
// kitchen_sink.s's own comment) to avoid goken's branch-threading
// optimization on an if/else-with-trailing-unconditional-jump shape.
TEXT _start(SB), $0
    MOV $5, R1
    MOV $5, R2
    CMP R2, R1
    CSEL EQ, R1, R2, R3
    CSET NE, R4
    CINC EQ, R1, R5
    MOV $0, R0
    TBNZ $0, R1, skip
    MOV $99, R0
skip:
    ADD R3, R4, R6
    ADD R5, R6, R6
    ADD R0, R6, R0
    MOV $93, R8
    SVC $0
