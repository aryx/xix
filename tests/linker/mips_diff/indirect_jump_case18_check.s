// Functional check for case 18's indirect-branch pair: "JAL 0(Rn)"
// (a call through a function pointer, goken's real optab.c row `{
// AJAL, C_NONE, C_NONE, C_ZOREG, 18, 4, REGLINK }` -- R31 implicitly
// gets the return address) and the already-wired "JMP (Rn)" (a
// plain goto through a register, REGZERO instead of REGLINK). See
// Parser_asmv.mly's own "con ireg" grammar comment and Codegenv.ml's
// own JAL{IndirectJump} case for why the leading "0" before the
// parens is real va grammar (unlike ARM32/ARM64's own xix-only "BL
// 0(Rn)" pipeline accommodation) yet always discarded, never
// encoded. Not byte-identical: any JAL (direct or indirect) hits the
// same pre-existing gap call.s's own case-11 already has (goken's
// sched.c hoists a real instruction into the delay slot instead of
// this port's plain NOP) -- confirmed this isn't a new gap specific
// to the indirect form by isolating a plain direct-JAL control case
// with the same 8-byte divergence. Found stress-testing real
// lib_core/libc (fmt/dofmt.c's real "JAL 0(R3)", a call through a
// Fmt struct's own flush-function-pointer field).
TEXT callee(SB), $0
    MOVW $11, R1
    RET

TEXT finish(SB), $0
    MOVW $4001, R2
    SYSCALL
    RET

TEXT _start(SB), $0
    MOVW $setR30(SB), R30
    MOVW $0, R4
    MOVW $callee(SB), R5
    JAL  0(R5)            // R1 = 11, R31 = return address
    MOVW $11, R6
    BEQ  R1, R6, ok1
    JMP  after1
ok1:
    ADD  $1, R4, R4
after1:
    MOVW $finish(SB), R8
    JMP  (R8)              // goto finish, exits with R4 (1 on success) as the exit code
