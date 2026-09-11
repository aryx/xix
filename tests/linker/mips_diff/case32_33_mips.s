// Float register arithmetic (case 32: ADDF/ADDD/SUBF/MULF/DIVF;
// case 33: ABSF/NEGD), byte-comparison only -- there's no way yet
// to load a meaningful value into a float register (case 30/31,
// MOVW between int/float registers, and case 34, float constant
// load, aren't ported: both need new AST for a float-register `gen`
// operand, unlike case 32/33 which already fit the existing ArithF
// AST). Every float register here starts at its power-on value
// (0), so this only confirms the encoding, not the arithmetic.
TEXT _start(SB), $0
    ADDF    F0, F1, F2
    ADDD    F0, F1, F2
    SUBF    F1, F2
    MULF    F0, F2
    DIVF    F1, F2, F3
    ABSF    F0, F1
    NEGD    F2, F3
    MOVW    $0, R4
    MOVW    $4001, R2
    SYSCALL
