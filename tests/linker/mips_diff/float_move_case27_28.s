// mov fr,[sl]ext/auto/oreg (case 28), float register store. Same
// ZOREG/LOREG/Entity split as int stores (case 7/35), F__ (single
// precision) only -- see Codegenv.ml's case 27/28 comment. Stores
// have no load-delay-slot hazard, so this stays byte-identical
// (unlike a float load -- see float_move_case27_28_check.s).
TEXT _start(SB), $0
    MOVW    $42, F0
    MOVF    F0, 0(R29)
    MOVF    F0, -8(R29)
    MOVW    $0, R4
    MOVW    $4001, R2
    SYSCALL
