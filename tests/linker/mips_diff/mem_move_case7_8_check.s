// Functional check for case 8 (mov soreg,r): store then read back
// through both ZOREG and LOREG addressing, self-checked with BEQ.
// Not byte-identical: case 8's mandatory load-delay-slot nop (see
// Codegenv.ml's comment) isn't always visible in goken's own output
// -- when something eligible sits right after, sched.c hoists it
// into the slot instead of a plain NOP, same as every other
// scheduler gap this session -- and this also inherits BEQ/JMP's
// own delay-slot gap (see branch_case6.s).
TEXT _start(SB), $0
    MOVW    $0, R4
    MOVW    $42, R1
    MOVW    R1, -8(R29)
    MOVW    R1, -4(R29)

    MOVW    -8(R29), R2
    MOVW    $42, R3
    BEQ     R2, R3, ok1
    JMP     after1
ok1:
    ADD     $1, R4, R4
after1:

    MOVW    -4(R29), R2
    BEQ     R2, R3, ok2
    JMP     after2
ok2:
    ADD     $2, R4, R4
after2:

    MOVW    $4001, R2
    SYSCALL
