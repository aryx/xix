// "MOV $sym+N(SB),R" -- address-of-a-global-with-a-nonzero-offset.
// A real, confirmed bug: this case's own `global`'s offset field used
// to be named `_offsetTODO` and was never added into any of the 3
// address formulas (SText2 forward-reference deferred lookup, SText2
// resolved case, SData2 fast/slow path) -- every such address-of with
// a nonzero offset silently computed the address of "sym+0" instead,
// discarding the offset entirely. Not caught by any earlier
// byte-identical .s fixture in this whole port (every prior
// address-of test happened to use offset 0); only found by running a
// REAL linked program under qemu and getting plausible-looking but
// WRONG output (fmt/dofmt.c's own "%d" digit-table setup, "MOV
// $.string<>+12(SB),R13", picking a sub-table partway into a larger
// shared string-literal blob -- see tests/linker/hello_libc_riscv/'s
// own test.sh for the fuller story). This fixture reproduces that
// exact shape directly: a single `.string<>`-style blob with several
// DATA lines, addressed at a nonzero offset partway through.
TEXT _start(SB), $0
    MOVW $setSB(SB), R3
    MOV $blob+12(SB), R8
    MOVB 0(R8), R9          // should read '0' (0x30) from offset 12
    MOVW R9, R10
    MOVW $93, R17
    ECALL
    DATA blob+0(SB)/8, $"xxxxxxxx"
    DATA blob+8(SB)/8, $"xxxx0123"
    DATA blob+16(SB)/8, $"456789ab"
    GLOBL blob(SB), $24
