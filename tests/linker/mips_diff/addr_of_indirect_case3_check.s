// Exercises case 3's "mov $soreg,r ==> or/add $i,o,r": "$off(Rbase)"
// as a Move2 source computes the EFFECTIVE ADDRESS Rbase+off into
// the destination register (an address-of, like C's "&x"), a real
// goken construct -- confirmed directly against real va, and against
// `linkers/vl/asm.c`'s own case 3 C code -- not a memory load. See
// Parser_asmv.mly's own grammar comment for why this bypasses Move2
// entirely (goken's real case-3 codegen is a plain ADDU-immediate,
// not representable as this port's own shared Ast_asm.ximm). Stores
// 77 at R29-8 via a plain register-offset destination, then reads it
// back through an address computed via "$-8(R29),R5" -- confirmed
// both toolchains agree on 77 as the resulting exit code. Not byte-
// identical: goken's own real assembler folds "compute address into
// R5, then immediately load through R5" straight into a single
// direct "LW R4,-8(R29)", skipping R5 entirely (confirmed by
// isolating: the case-3 instruction alone is already byte-identical,
// and adding the following load costs goken 0 extra bytes but this
// port 8) -- a real peephole optimization in goken's own assembler
// this port doesn't replicate, same general category as the sched.c
// delay-slot-hoisting gap other _check fixtures document. Found
// stress-testing real lib_core/libc (fmt/nan64.c's real "MOVW
// $4(R29),R2").
TEXT _start(SB), $16
    MOVW $setR30(SB), R30
    MOVW $77, R1
    MOVW R1, -8(R29)
    MOVW $-8(R29), R5
    MOVW 0(R5), R4
    MOVW $4001, R2
    SYSCALL
