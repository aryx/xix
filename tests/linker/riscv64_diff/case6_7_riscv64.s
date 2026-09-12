// Byte/half memory access (case 6/7's B_/H_ variants, generalizing
// the store/load helpers originally wired only for RLINK's W__/V__
// save/restore). Also exercises the new `con TOPAR reg TCPAR ->
// Indirect` grammar rule (mirrors Parser_asmv.mly/MIPS), needed for
// user-written "offset(reg)" memory operands at all -- previously
// Indirect was only ever constructed programmatically by
// Rewritei.ml's prologue/epilogue, unreachable from a .s file.
//
// Uses the reserved stack space below R2 (a nonzero-autosize leaf
// frame -- Rewritei.ml's "leaf+locals" case, SP-adjust-only
// prologue) as scratch memory, rather than a GLOBL/DATA location:
// an ad-hoc attempt to write through an SB-relative address here
// segfaulted identically on both goken and xix (same bytes, same
// crash), suggesting that specific data-segment-write path isn't
// exercised correctly by *either* toolchain yet and predates this
// port -- not something to chase while porting case 6/7 specifically.
TEXT _start(SB), $16
    MOVW    $-1, R9
    MOVB    R9, 0(R2)       // store byte 0xFF
    MOVW    $0x1234, R9
    MOVH    R9, 4(R2)       // store half 0x1234

    MOVB    0(R2), R4       // sign-extend load: 0xFF -> -1
    MOVBU   0(R2), R5       // zero-extend load: 0xFF -> 255
    MOVH    4(R2), R7       // sign-extend load: 0x1234 -> 0x1234 (positive)
    MOVHU   4(R2), R8       // zero-extend load: 0x1234 -> 0x1234

    // fold into the exit code: R4=-1(0xff) R5=255 R7=0x1234 R8=0x1234
    ADD     R4, R5, R10
    ADD     R10, R7, R10
    ADD     R10, R8, R10

    MOVW    $93, R17
    ECALL
