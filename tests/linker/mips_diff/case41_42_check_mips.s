// Functional check for case 42 (MFCC1, read): write then read back
// FCR31, self-checked with BEQ. Not byte-identical: case 42's
// mandatory 2-NOP delay slot (see Codegenv.ml's comment) plus
// BEQ/JMP's own delay-slot gap (see case6_mips.s).
TEXT _start(SB), $0
    MOVW    $0, R4
    MOVW    $42, R1
    MOVW    R1, FCR31
    MOVW    FCR31, R2
    MOVW    $42, R3
    BEQ     R2, R3, ok1
    JMP     after1
ok1:
    ADD     $1, R4, R4
after1:
    MOVW    $4001, R2
    SYSCALL
