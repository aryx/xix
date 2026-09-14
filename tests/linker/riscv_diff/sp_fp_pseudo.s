// Plan9 "off(SP)"/"off(FP)" pseudo-frame addressing (goken's own
// D_AUTO/D_PARAM, "case 12-16" for the load/store side, plus its own
// "$sym+N(SP)"/"$sym+N(FP)" address-of form) -- completely
// unimplemented before, needed by nearly every real function with
// local variables or named arguments (not just leaf-level ones).
// This port's own `Param`/`Local` AST names are swapped relative to
// goken's own D_PARAM/D_AUTO naming (Param<->"SP" token<->goken's
// D_AUTO; Local<->"FP" token<->goken's D_PARAM) -- see Codegeni.ml's
// own `true_autosize`/`resolve_entity` comment for the full story.
//
// Also exercises a genuine, separate, pre-existing bug found while
// testing this: Rewritei.ml's own RET-rewrite case 2 (a LEAF function
// *with* a nonzero declared frame) updated `n.next` but never
// `n.instr` itself, leaving the original un-transformed virtual RET
// node behind for codegen to choke on ("rewrite should have
// transformed virtual instrs") -- any such function was already
// broken regardless of the Local/Param work above; this fixture (a
// leaf function with a real $32 frame) exercises that exact shape.
TEXT _start(SB), $32
    MOVW $setSB(SB), R3
    MOVW $42, R9
    MOVW R9, x-8(SP)        // store to a local (goken's own D_AUTO)
    MOVW x-8(SP), R10       // reload -- R10 = 42
    MOV $x-8(SP), R11       // address-of a local
    MOVW 0(R11), R12        // load through it -- R12 = 42
    ADD R12, R10, R8        // R8 = 84
    MOVW R8, R10
    MOVW $93, R17
    ECALL
