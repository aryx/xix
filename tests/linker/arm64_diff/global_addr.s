// Exercises the literal-pool mechanism for address-of-global (and,
// transitively, the SB-relative fast-path load): "MOV $sym(SB),R" has
// no dedicated ADD-from-SB instruction on this arch (unlike ARM32/
// MIPS/RISC-V) -- the assembler can't know the link-time absolute
// address ahead of time, so goken always routes it through a
// PC-relative literal-pool load (see Codegen7.ml's `pool` type and
// Layout7.ml's own comment). Two separate address-of-global loads
// here (setSB and foo) confirm goken never deduplicates pool values:
// each gets its own, separately 8-byte-aligned pool entry, even
// though both are 8-byte DWORDs sharing the same flush.
//
// Also exercises the standard "MOV $setSB(SB),R28" bootstrap every
// SB-relative fixture needs on this arch (mirrors MIPS's
// "MOVW $setR30(SB),R30" / RISC-V's "MOVW $setSB(SB),R3" convention)
// -- REGSB (X28) has no bias here (setSB is defined at plain
// data-offset 0), unlike ARM32's BIG=4092 or RISC-V's BIG=2048.
TEXT _start(SB), $0
    MOV $setSB(SB), R28
    MOV foo(SB), R1
    MOV $foo(SB), R2
    MOV 0(R2), R3
    ADD R3, R1, R0
    MOV $93, R8
    SVC $0
DATA foo+0(SB)/8, $50
GLOBL foo(SB), $8
