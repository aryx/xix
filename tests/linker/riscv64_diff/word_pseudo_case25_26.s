// case 25 (WORD $x) and case 26 (pseudo ops: TEXT/GLOBL/DATA) --
// both already routed through the arch-independent
// Codegen.default_rules (the `T.Virt _ | T.TEXT _ | T.WORD _ ->
// Codegen.default_rules ...` catch-all at the top of Codegeni.ml's
// `rules`), the same shared code path already exercised byte-for-
// byte by ARM/MIPS -- this fixture just confirms there's nothing
// RISC-V-specific needed, mirroring MIPS's case 0/case 40
// "confirmed no new code needed" precedent.
TEXT _start(SB), $0
    MOVW    $93, R17
    MOVW    $7, R10
    ECALL

GLOBL x(SB), $8
DATA x(SB)/4, $42
DATA x+4(SB)/4, $43
WORD $1234
