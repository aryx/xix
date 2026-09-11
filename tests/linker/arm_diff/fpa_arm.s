// Legacy ARM FPA (coprocessor 1) floating point (case 54): ADDF with
// an immediate constant from goken's chipfloats[] table (0,1,2,3,4,5,
// 0.5,10 -- see float.c), plus CMPF to exercise the compare form.
// FPA is defunct hardware (no chip has implemented it since the
// ARM7500FE era) and goken defaults to it (vfp=debug['f'], off
// unless -f is passed) -- this is purely an encoding-parity test;
// confirmed empirically that qemu-arm has no FPA coprocessor to
// trap/emulate it: both sides SIGILL identically (rc=132) on the
// very first FPA instruction, which is itself the expected,
// matching signal here (same crash, not a different one).
TEXT _start(SB), $0
    ADDF    $1.0, F0, F1
    MULF    $2.0, F1, F2
    CMPF    F1, F2
    MOVW    $1, R7
    SWI     $0
