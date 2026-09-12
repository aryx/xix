// Functional check for case 47/48: the real LL-then-SC protocol (LL
// establishes a link, SC then conditionally stores), then a plain
// load (case 8) reads back whatever ended up in memory and
// self-checks with BEQ. Not byte-identical: LL immediately followed
// by something eligible gives goken's sched.c something to hoist
// into the load-delay slot instead of a plain nop (see
// case47_48_mips.s's comment), and this also inherits BEQ/JMP's own
// delay-slot gap (see case6_mips.s).
TEXT _start(SB), $0
    MOVW    $0, R4
    MOVW    $42, R2
    LL      0(R29), R1
    SC      R2, 0(R29)
    MOVW    0(R29), R1
    MOVW    $42, R3
    BEQ     R1, R3, ok1
    JMP     after1
ok1:
    ADD     $1, R4, R4
after1:
    MOVW    $4001, R2
    SYSCALL
