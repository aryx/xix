// MOVW between an int and a float register (case 30, MTC1; case
// 31, MFC1). Both have a mandatory MIPS I COP1 transfer delay
// slot, just like a branch -- see the comment in Codegenv.ml. Kept
// right before SYSCALL here with nothing else around: SYSCALL isn't
// hoistable into the slot, so goken pads a plain NOP too (confirmed
// via `vl -a`), keeping this fixture byte-identical. See
// float_int_move_case30_31_check.s for a functional check that chains MTC1
// into a same-register MFC1 (real hoisting happens there, so it's
// not byte-identical).
TEXT _start(SB), $0
    MOVW    $42, R1
    MOVW    R1, F0
    MOVW    F1, R2
    SYSCALL
