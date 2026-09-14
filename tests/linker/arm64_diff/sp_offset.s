// Exercises a real "(SP)" reference -- this port's own swapped-naming
// A.Param constructor (see local_param_offset's own comment in
// Codegen7.ml) -- resolving to the correct absolute address within
// THIS function's own frame, in a *non-leaf* function (so autosize
// gets the codegen-visible "-pcsz" deflation the formula needs to
// correctly undo -- a leaf-with-declared-size-0 function, the only
// case any prior fixture exercised indirectly, never hits that
// deflation at all, which is exactly why this class of bug slipped
// through). The final MOV cross-checks the named local's real
// address against a raw RSP-relative reference (a pure store/load
// round-trip through "x-8(SP)" alone wouldn't catch a consistently-
// wrong offset) -- verified directly against real goken with a
// hand-written probe before fixing this port's own formula.
TEXT stub(SB),0,$0
    RET R30

TEXT _start(SB),0,$16
    MOV $42,R1
    MOV R1,x-8(SP)
    BL stub(SB)
    MOV 24(RSP),R0
    MOV $93,R8
    SVC $0
