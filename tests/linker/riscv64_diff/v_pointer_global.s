// "MOV R,sym(SB)"/"MOV sym(SB),R" -- a bare, pointer-width store/load
// to a global, a V__ sibling of the already-wired W__ case, genuinely
// is_64-dependent (SD/LD instead of SW/LW) unlike W__'s own
// unconditional "always 32-bit" funct3. Found stress-testing real
// lib_core/libc on riscv64 specifically (port/mainargs.c's own real
// "MOV R9,_mainargv(SB)", storing a real 64-bit pointer).
//
// The V__ sibling of the already-wired W__ "MOV $0,off(R)" zero-
// immediate store (also needed by this same session, fmt/dofmt.c's
// own real "MOV $0,16(R2)") has no fixture of its own here: real
// goken's own `ja` rejects this exact bare-immediate spelling
// outright for V__ too (same reasoning as W__'s own case -- see
// Codegeni.ml's own comment), so there's no goken reference to
// byte-compare against; verified instead by the closure itself
// (tests/linker/hello_libc_riscv64/) linking and running correctly.
TEXT _start(SB), $0
    MOVW $setSB(SB), R3
    MOV $99, R9
    MOV R9, foo(SB)         // V__ store-to-global (SD)
    MOV foo(SB), R10        // V__ load-from-global (LD) -- R10 = 99
    MOVW R10, R10
    MOVW $93, R17
    ECALL
DATA foo+0(SB)/8, $0
GLOBL foo(SB), $8
