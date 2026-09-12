// mov r,fcr (case 41, MTCC1 -- write, via a dummy MFCC1 read into
// R0 first, a real MIPS I FCR hazard workaround). No delay-slot
// hazard for the write, so this stays byte-identical (unlike case
// 42's read, which gets the same unconditional 2-NOP D_FCREG
// special case as case 38's MFC0 -- see case41_42_check_mips.s and
// Codegenv.ml's case 41/42 comments).
TEXT _start(SB), $0
    MOVW    $42, R1
    MOVW    R1, FCR31
    MOVW    $0, R4
    MOVW    $4001, R2
    SYSCALL
