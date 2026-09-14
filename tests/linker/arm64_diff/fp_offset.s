// Exercises a real "(FP)" reference -- this port's own swapped-naming
// A.Local constructor (see local_param_offset's own comment in
// Codegen7.ml for the full naming explanation) -- resolving to the
// correct absolute address in the CALLER's own frame for a genuine
// stack-passed parameter. No existing fixture exercised A.Local/
// A.Param at all before this one, which is exactly how a real bug in
// the offset formula (env.autosize + 8 + off, silently correct only
// for this callee's own trivial $0-locals case) slipped past every
// regression test *and* past the full hello_libc closure linking
// successfully -- it only surfaced as a NULL-pointer segfault at
// actual native runtime. Verified directly against real goken with a
// hand-written probe before fixing: a leaf callee's own "arg+0(FP)"
// resolves to the caller's SP-at-call + 8, confirmed by cross-
// checking against a raw RSP-relative store at the same offset.
TEXT callee(SB),0,$0
    MOV arg+0(FP),R0
    ADDW R0,R0,R0
    RET R30

TEXT _start(SB),0,$16
    MOV $21,R1
    MOV R1,8(RSP)
    BL callee(SB)
    MOV $93,R8
    SVC $0
