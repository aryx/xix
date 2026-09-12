// Exercises DMB/DSB/ISB (case 51) -- memory/instruction barriers,
// no-ops in this single-threaded userspace harness but real,
// byte-verified encodings (goken's own SYSOP-based opirr() table).
// Bare NOP and HINT are deliberately not wired -- see Ast_asm7.ml's
// Barrier comment (NOP is dead code in goken's own reference
// implementation, deleted by noop.c before ever reaching asmout.c).
TEXT _start(SB), $0
    DMB $15
    DSB $15
    ISB $15
    MOV $42, R0
    MOV $93, R8
    SVC $0
