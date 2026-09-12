// Generalizes case 2 (previously ADDI-only) to the rest of the
// OP-IMM family goken's optab reuses the same funct3 for as their
// register-register counterparts: ANDI/ORI/XORI/SLTI/SLTIU. Also
// validates case 10's MOVBU-via-ANDI encoding indirectly, since it
// reuses this exact same op_itype/op_opimm/funct3=7 path (goken's
// own dedicated register-to-register MOVBU syntax can't be
// differentially tested at all -- see case10's comment in
// Codegeni.ml -- so this is the closest byte-identical confirmation
// available for that shared encoding).
TEXT _start(SB), $0
    MOVW    $0xff, R2
    AND     $0x0f, R2, R3    // R3 = 0x0f = 15
    OR      $0xf0, R2, R4    // R4 = 0xff = 255 (already all set)
    XOR     $0xff, R2, R5    // R5 = 0x00 = 0
    SLT     $0x100, R2, R6   // R6 = 1 (0xff < 0x100, signed)
    SLTU    $0x100, R2, R7   // R7 = 1 (0xff <u 0x100, unsigned)

    ADD     R3, R4, R10
    ADD     R10, R5, R10
    ADD     R10, R6, R10
    ADD     R10, R7, R10

    MOVW    $93, R17
    ECALL
