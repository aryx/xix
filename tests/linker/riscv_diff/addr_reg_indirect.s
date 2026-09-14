// "MOV $8(R2),R9" -- address-of an arbitrary-register indirect (NOT
// a memory load: real goken compiles this straight to a plain "ADDI
// R9,R2,8", no memory access at all -- verified by hand-decoding real
// ia/il's own output bytes). Genuinely different from `ximm`'s own
// `Address of A.entity` (which only covers the 3 pseudo-registers
// SB/SP/FP, not an arbitrary real register) -- wired as its own
// top-level grammar production straight to `Arith`, mirroring
// Parser_asmv.mly's own identical MIPS production, confirmed to be
// the exact same real construct from the exact same real source file
// (fmt/nan64.c's own "MOVW $4(R29),R2" on MIPS, "MOV $8(R2),R9" here).
TEXT _start(SB), $0
    MOVW $50, R2
    MOV $8(R2), R9          // R9 = R2 + 8 = 58
    MOVW $6, R10
    MOV $-6(R9), R11        // R11 = R9 - 6 = 52
    SUB R10, R11, R10       // R10 = 52 - 6 = 46
    MOVW $93, R17
    ECALL
