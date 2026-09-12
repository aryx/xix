// Functional check for RET on a leaf procedure that still declares a
// nonzero frame ("leaf with locals" -- Rewritev.ml's own case 2,
// mirroring goken's vl/noop.c's ATEXT/ARET 3-way leaf/frame shape,
// not a numbered asm.c case). Confirmed as a real, previously-latent
// bug (not just theoretical): before this fixture's fix, Rewritev.ml
// always expanded RET as "MOVW 0(SP),R2; ADD $autosize,SP; JMP (R2)"
// regardless of leaf-ness, but the matching prologue only ever wrote
// RLINK to 0(SP) for a NON-leaf TEXT -- for a leaf-with-locals
// procedure like `addone` below, 0(SP) was never initialized, so R2
// got loaded with garbage and jumped to (a real segfault, verified
// by temporarily reverting the fix and re-running this exact fixture
// under qemu-mips). goken's own vl/noop.c never saves/restores RLINK
// for a leaf at all (only the SP adjustment), confirmed by reading
// its ATEXT/ARET cases directly.
//
// Not byte-identical: goken's sched.c always hoists the epilogue's
// SP-restore ADD into the RET's own JMP delay slot (confirmed
// empirically here, and separately for the non-leaf/case-3 RET shape
// too), which this port doesn't replicate -- same documented
// scheduler-gap category as every other _check fixture in this
// directory (see Codegenv.ml's `nop` comment). Rewritev.ml always
// emits ADD-then-JMP in program order (matching goken's own noop.c
// pre-scheduling shape, just never its post-scheduling one).
TEXT addone(SB), $4
    MOVW $1, R2
    MOVW R2, 0(R29)
    MOVW 0(R29), R3
    ADD R1, R3, R1
    RET

TEXT _start(SB), $0
    MOVW $setR30(SB), R30
    MOVW $20, R1
    JAL addone(SB)
    MOVW $21, R3
    BEQ R1, R3, ok
    MOVW $1, R4
    JMP done
ok:
    MOVW $0, R4
done:
    MOVW $4001, R2
    SYSCALL
