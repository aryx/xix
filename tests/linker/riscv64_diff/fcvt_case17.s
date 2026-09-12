// case 17: "fcvt S,D" -- covers all 6 real conversion directions
// (float<->double via MOVFD/MOVDF, and int<->float/double via
// MOVFW/MOVDW/MOVWF/MOVWD), split at the AST level by register-file
// direction (see Ast_asmi.ml's FCVTFF/FCVTFI/FCVTIF comment). The
// (funct7, rs2-field, rounding-mode) triple per direction was
// verified empirically against real goken -- easy to get wrong
// reading asm.c's case 17 body alone, since the funct7 term
// (`o->param<<25`) is buried in the OP_RF macro's own definition, a
// screen away from the case body that uses it.
//
// R5=42 -> int->float->double->float->int round-trips exactly back
// to 42 (float can represent small integers exactly); R6=77 ->
// int->double->int round-trips the same way via the other 2
// directions (MOVWD/MOVDW) not otherwise exercised above.
TEXT _start(SB), $0
    MOVW $42, R5
    MOVWF R5, F1
    MOVFD F1, F2
    MOVDF F2, F3
    MOVFW F3, R10

    MOVW $77, R6
    MOVWD R6, F4
    MOVDW F4, R11

    ADD R10, R11, R10
    MOVW $93, R17
    ECALL
