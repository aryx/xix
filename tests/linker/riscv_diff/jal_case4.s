// Unconditional jump-and-link-to-label (case 4, the JAL half): a
// forward JAL over exactly one instruction, using an explicit link
// register (not RLINK) -- xix's own grammar routes "JAL reg,label"
// through the JALR AST constructor (Parser_asmi.mly: `TJAL reg TC
// branch {JALR($2,$4)}`), which is why Codegeni.ml's case 4 match
// arm for a label-shaped branch operand is
// `JALR (rd, {contents=Absolute _})`, not a separate "JAL with
// explicit register" constructor -- goken's own "JAL" mnemonic
// always requires an explicit register too (its grammar tokenizes
// "JAL"/"JALR" identically, both needing a comma-register; a bare
// "JAL label" with no register doesn't even parse in goken's own
// assembler).
//
// Unlike a plain "JMP label", goken's assembler treats an
// unconditional JAL-with-link as a "call that returns" and does NOT
// drop the code it jumps over as dead, even though nothing here ever
// actually returns to it (R8 is set but never used to jump back) --
// confirmed by the ADD below surviving byte-for-byte in goken's own
// output.
//
// case 4's plain-JMP-to-label form, and JMP's interaction with
// conditional branches/loops, is deliberately NOT covered by a
// byte-identical fixture at all (no companion _check fixture either,
// unlike the usual convention -- see tests/linker/README.md):  every
// natural shape collides with one of goken's own assembler-side
// code-layout optimizations (dead-code-after-JMP elision, the
// branch+jmp inversion already documented in branch_case3.s, and a
// "loop rotation" that, combined with a later JAL/indirect-return in
// the same function, was observed via an ad-hoc test to miscompute a
// target address into a non-4-byte-aligned garbage branch -- well
// out of scope, and not worth a fixture that would just document a
// crash).
TEXT _start(SB), $0
    MOVW    $1, R10
    JAL     R8, lbl2
    ADD     R10, R10, R10   // skipped at runtime, but kept in goken's
                             // own output -- see comment above
lbl2:
    MOVW    $93, R17
    ECALL
