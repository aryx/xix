// Multi-register load/store (MOVM -> STM/LDM): case 38 (store,
// "MOVM [regs],(Rbase)") and case 39 (load, "MOVM (Rbase),[regs]").
// ".DB.W"/".IA.W" are the real push/pop idiom (P/U/W address-mode
// suffix bits) -- see Ast_asm5.ml's movm_addr_mode comment for why
// these bits are what finally justified building a real generic
// dot-suffix-flag grammar mechanism (Parser_asm5.mly's `condf`/TSUF),
// unlike PSR's lone ".F" bit (case 35/36/37) which wasn't worth it on
// its own.
//
// Uses R13 (the real stack pointer, already valid at Linux process
// entry) as the base register. Two separate push/pop pairs to
// exercise both reglist grammar shapes -- goken's own a.y grammar
// does NOT allow combining a range with a trailing list in one
// reglist (confirmed against goken directly: "[R4-R6,R14]" is a
// syntax error on both sides), so a plain comma list and a register
// range are tested separately here, each self-balanced (same
// registers pushed and popped) rather than combined.
TEXT _start(SB), $0
    MOVW    $10, R4
    MOVW    $20, R5
    MOVW    $30, R14
    MOVM.DB.W [R4,R5,R14], (R13)
    MOVW    $0, R4
    MOVW    $0, R5
    MOVW    $0, R14
    MOVM.IA.W (R13), [R4,R5,R14]
    ADD     R5, R4, R4
    ADD     R14, R4, R4

    MOVW    $1, R6
    MOVW    $2, R7
    MOVW    $3, R8
    MOVM.DB.W [R6-R8], (R13)
    MOVW    $0, R6
    MOVW    $0, R7
    MOVW    $0, R8
    MOVM.IA.W (R13), [R6-R8]
    ADD     R7, R6, R6
    ADD     R8, R6, R6
    ADD     R6, R4, R4

    MOVW    R4, R0
    MOVW    $1, R7
    SWI     $0
