// "MOVW R,sym(SB)" / "MOVW sym(SB),R" -- storing/loading a global's
// *value* by symbol (as opposed to "MOVW $sym(SB),R", address-of,
// case 9/11/20). Needed its own `gen` grammar rule (`name -> Entity`,
// previously only ever constructed internally by Rewritei.ml) and
// its own Codegeni.ml case entirely -- this whole shape was
// completely unimplemented before. goken's own assembler resolves
// this small-offset case straight to case 6/7 (RSB is just another
// register once biased by BIG, confirmed via optab.c: the same
// C_SOREG class a plain "sb R,I(S)" and a small-offset "sb R,sym(SB)"
// both resolve to) -- see sb_value_case12_13.s for the large-offset
// slow-path sibling.
//
// A real bug was found and fixed while writing this: "MOVW" always
// means 32-bit width regardless of arch (confirmed against goken's
// own optab.c: `AMOVW,...,OSTORE,2` is unconditional, not is_64-
// gated) -- Codegeni.ml's pre-existing case 6/7 helper is_64-branches
// its own funct3 only because it's *also* reused internally by
// Rewritei.ml's pointer-width RLINK save (never reachable from real
// .s source), which this new, directly-user-reachable case must NOT
// inherit.
TEXT _start(SB), $0
    MOVW $setSB(SB), R3
    MOVW $42, R6
    MOVW R6, foo(SB)
    MOVW foo(SB), R10
    MOVW $93, R17
    ECALL
DATA foo+0(SB)/4, $0
GLOBL foo(SB), $4
