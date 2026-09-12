// Move to (case 56) and from (case 57) FPSR, the FPA status
// register: goken dispatches this through the exact same "MOVW"
// mnemonic/gen mechanism as an ordinary int move (a.y's `gen: ... |
// LFCR`), not a distinct mnemonic like MOVF/MOVD, so it's handled as
// a new mov_operand alternative (FCRImsr) on the existing MOVE instr
// rather than a new top-level instr constructor -- see Ast_asm5.ml's
// FCRImsr comment.
//
// FPSR is genuinely accessible from user mode under qemu-arm (write
// then read back round-trips correctly into the exit code). FPCR
// (the same case 56/57 encoding, just fcr_val=2 instead of 1) was
// also verified via an ad-hoc, non-committed test: byte-identical
// against goken, and traps as "Illegal instruction" identically on
// both sides under qemu-arm (apparently not emulated) -- confirms
// the shared gfcr helper's encoding but adds no further coverage
// beyond what this fixture already exercises, so not added as its
// own fixture.
TEXT _start(SB), $0
    MOVW    $5, R0
    MOVW    R0, FPSR
    MOVW    FPSR, R1
    MOVW    R1, R0
    MOVW    $1, R7
    SWI     $0
