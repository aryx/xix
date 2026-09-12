// Standalone LUI (case 8): a MOVW-immediate whose low 12 bits are
// exactly zero. goken's own constant classifier (il/span.c's
// `aclass`, the C_UCON class) picks a 4-byte LUI-only encoding for
// this shape instead of case 9's general 8-byte LUI+ADDI -- there's
// nothing to add, so no ADDI is emitted. Discovered while trying to
// test xix's own standalone `LUI $I,D` mnemonic directly: goken's
// grammar parses "LUI" (LLUI -> ALUI) but its own linker optab.c has
// no entry for ALUI at all ("illegal combination LUI..."), i.e.
// goken's dedicated LUI mnemonic is dead syntax -- case 8 is only
// ever reachable in practice through this MOVW-immediate
// classification, so that's what this fixture (and
// Codegeni.ml's fast path in the `Move2 (W__, Right (Int i), ...)`
// case) actually exercises. xix's own `LUI $I,D` mnemonic (wired in
// Parser_asmi.mly/Parse_asmi.ml, same reasoning as case4's bare
// "JAL label" being valid xix syntax with no goken equivalent) is
// kept as a real, directly-usable building block even though it has
// no matching goken syntax to differentially test against.
TEXT _start(SB), $0
    MOVW    $0x2a000000, R10   // low 12 bits are zero -> case 8 (LUI only)
    SRL     $20, R10, R10      // R10 = 0x2a0 = 672
    MOVW    $93, R17
    ECALL
