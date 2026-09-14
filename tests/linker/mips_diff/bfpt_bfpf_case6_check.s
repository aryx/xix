// Exercises case 6's BFPT/BFPF ("branch if the FP condition flag is
// true/false", set by a preceding CMPEQ_/CMPGE_/CMPGT_) -- real
// goken grammar (`LTYPEG comma rel` in assemblers/va/a.y), no
// register operand at all, unlike every other case-6 branch. Two
// real bugs found and fixed via this exact probe:
//
// 1. BFPT/BFPF's own sub-opcode selector: goken packs it as one flat
//    literal (SP(2,1)|(257<<16) for BFPT, SP(2,1)|(256<<16) for
//    BFPF), which straddles a field-width boundary this port's own
//    Bits.t sanity checker infers from neighboring declared bit
//    positions -- had to be re-split as two non-overlapping
//    sub-fields (bit24 + bit16) to even LINK at all. See Codegen.ml's
//    own opirr_bfp_opcode comment for the exact bit-level derivation.
// 2. CMPEQ_/CMPGE_/CMPGT_'s own case 32 codegen: the first attempt
//    (reasoned from C source alone) looked correct but silently
//    computed the wrong comparison result -- goken's own C passes
//    `p->to.reg` as a third OP_FRRR argument for compares too, but a
//    real MIPS FP compare has no third register field there at all
//    (that bit range is the condition-code selector, not a
//    register). Caught only by decoding real goken's own linked
//    bytes for "CMPEQD F2,F4" and finding an extra, wrong bit. See
//    ArithF's own CMPEQ_/CMPGE_/CMPGT_ dispatch comment.
//
// Not byte-identical (goken's own real MOVWD/CMPxxD scheduling and
// this port's own xix-only REGTMP-heavy D__ conversion expansion
// diverge in size), hence _check -- only the functional result
// (exit 42) is compared. Also exercises MOVWD (word->double
// conversion, case 33's FCvt family) as the vehicle for getting two
// equal doubles into F2/F4 without a bare "MOVD $2.0,Fn" float
// literal (goken's own real assembler routes at least some of its 8
// "chipfloat" values through a genuine literal pool this port
// doesn't replicate -- a separate, already-known gap, not what this
// fixture is about).
TEXT _start(SB), $0
    MOVW $setR30(SB), R30
    MOVW $2, R1
    MOVW R1, F2
    MOVWD F2, F2
    MOVW R1, F4
    MOVWD F4, F4
    CMPEQD F2, F4
    BFPF fail
    CMPGED F4, F2
    BFPF fail
    MOVW $42, R4
    JMP done
fail:
    MOVW $0, R4
done:
    MOVW $4001, R2
    SYSCALL
