// Broader coverage in one straight-line program: register-register and
// register-immediate arithmetic, shift, and compare+conditional
// branch. Mirrors the spirit of arm_diff/kitchen_sink.s (ARM32).
//
// The conditional-branch shape here is deliberately a single
// "skip one block, no matter which way" pattern (a bare BNE over one
// instruction, nothing that also needs its own trailing unconditional
// jump) -- an earlier version used a classic if/else shape (BEQ to a
// label, false-path code, an unconditional "B done" past the true
// path, then the true-path label), and goken's own assembler/linker
// turned out to apply a real branch-threading optimization to that
// exact shape: it eliminated the "B done" by inlining "done"'s code
// right where the jump was, and relocated the true-path block after
// it with a corrected back-branch -- confirmed by direct byte
// comparison, not guessed. This is the same kind of goken-side
// assembler/linker cleverness ARM32's kitchen_sink.s ran into (see
// arm_port.md's own writeup: "something in 5a/5l's asm/noop layer
// decides to duplicate short tails instead") -- not chased here
// either, same call: rewrite the fixture to a shape that doesn't
// trigger it, rather than replicate the optimization.
//
// Immediate values are also deliberately chosen to avoid goken's own
// "MOV $con,R" literal-pool routing for values that are also valid
// ARM64 logical-immediate ("bitcon") bit patterns (e.g. 1/2/3/4/7 --
// see Codegen7.ml's move_immediate_encoding comment) -- not yet
// implemented (needs a literal pool, a follow-up). CMP/ADD/SUB's own
// immediate operand has no such restriction (confirmed directly
// against goken).
TEXT _start(SB), $0
    MOV $10, R1
    MOV $11, R2
    MUL R2, R1, R3      // R3 = 10*11 = 110
    ADD $9, R3, R3      // R3 = 119
    SUB $13, R3, R3     // R3 = 106
    LSL $2, R1, R4      // R4 = 10<<2 = 40
    ADD R4, R3, R5      // R5 = 146

    MOV $200, R0
    CMP $146, R5
    BNE skip
    MOV $246, R0        // only taken if the CMP/BEQ path is correct
skip:
    MOV $93, R8
    SVC $0
