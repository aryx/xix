// case 15/16: "mov r,L(s)" / "mov L(s),r" -- store/load through an
// arbitrary base register at a large offset that doesn't fit ADDI's
// 12-bit field (unlike case 12/13's SB-specific slow path, no
// INITDAT/BIG bias here: goken's `OP_ADD(r,REGTMP,REGTMP)` just adds
// the base register at runtime). Shares Codegeni.ml's generalized
// gen_store/gen_load helpers with case 6/7 (small offset) -- the same
// functions now transparently take either path based on whether the
// offset fits.
//
// A real bug was found and fixed while writing this: Rewritei.ml's
// own link-register save/restore used move_size W__ ("MOVW", always
// 32-bit) when it actually wants pointer-width behavior (a bare "MOV"
// -- 8 bytes on riscv64) -- conflating the two was harmless as long
// as nothing else routed a real 32-bit "MOVW" through the exact same
// encoder arm with an is_64-dependent funct3, which case 15/16 (via
// this fixture, a genuine large-offset "MOVW" on riscv64) finally
// did. Fixed by giving W__ and V__ their own separate arms in
// Codegen.ml, and having Rewritei.ml use V__ (not W__) for RLINK.
TEXT _start(SB), $8200
    MOVW $55, R6
    MOVW R6, 8192(R2)
    MOVW 8192(R2), R10
    MOVW $93, R17
    ECALL
