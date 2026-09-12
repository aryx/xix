// ADD $con,r1,r2 at the UCON boundary (case 25, LU+ADD, low 16 bits
// zero) and beyond it into genuine LCON territory (case 23,
// LU+OR+ADD). Both confirmed via `vl -a` to be the plain
// REGTMP-based expansion, unlike MOVW's case 19 literal-pool
// surprise (see case 24's comment in Codegenv.ml).
TEXT _start(SB), $0
    MOVW    $1, R1
    ADD     $65536, R1, R2      // case 25: R2 = 1+65536 = 65537
    ADD     $305419896, R1, R3  // case 23: R3 = 1+305419896 = 305419897
    SUB     R2, R3, R4          // R4 = R3-R2 = 305354360 -> exit&0xff
    MOVW    $4001, R2
    SYSCALL
