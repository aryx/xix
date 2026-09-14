// BLE/BGT/BLEU/BGTU -- real goken accepts these 4 mnemonics (an
// earlier, now-corrected comment in this port claimed it didn't;
// confirmed against goken's own lex.c, which maps all 4 to real
// tokens), rewritten by goken's own linker (linkers/il/obj.c, case
// ABGT/ABGTU/ABLE/ABLEU) into the reversed-relation LT/GE hardware
// form with BOTH the condition and the two register roles reversed --
// not just a condition remap. A first implementation attempt here
// remapped only the condition (reusing the already-computed rs1/rs2
// unchanged), producing the right *size* but wrong *content*, caught
// by hand-decoding real bytes, not by trusting the derivation.
// Chained into the exit code the same way branch_case3.s's own
// fixture is (each taken check, using the inverted condition to skip
// a single following ADD, adds a distinct power of two).
TEXT _start(SB), $0
    MOVW    $5, R8
    MOVW    $10, R9
    MOVW    $0, R10

    MOVW    $1, R4
    BGT     R9, R8, skip1
    ADD     R10, R4, R10
skip1:

    MOVW    $2, R4
    BLE     R8, R9, skip2
    ADD     R10, R4, R10
skip2:

    MOVW    $4, R4
    BGTU    R9, R8, skip3
    ADD     R10, R4, R10
skip3:

    MOVW    $8, R4
    BLEU    R8, R9, skip4
    ADD     R10, R4, R10
skip4:

    MOVW    $93, R17
    ECALL
