// Move from PSR (case 35, MRS), to PSR from a register (case 36,
// MSR), and to PSR from an immediate (case 37, MSR) -- same "MOVW"
// mnemonic/gen mechanism as the FP[CS]R moves (case 56/57), see
// Ast_asm5.ml's PSRImsr/psrreg comment (the ".F" flags-only suffix
// isn't wired: xix's grammar has no dot-suffix-flag parsing
// mechanism at all yet, so this always emits the "full PSR write"
// encoding).
//
// CPSR is genuinely accessible from user mode under qemu-arm (write
// then read back is observable in the exit code, though qemu's own
// user-mode CPSR emulation only preserves/exposes some of the bits).
// SPSR (same case 35/36/37 encoding, just psr_bit=1 instead of 0)
// was also verified via an ad-hoc, non-committed test: byte-
// identical against goken, and traps as "Illegal instruction"
// identically on both sides (no SPSR in user mode) -- confirms the
// shared gpsr_read/gpsr_write_base encoding but adds no further
// coverage, so not added as its own fixture, same reasoning as
// fcr_move_case56_57.s's FPCR aside.
TEXT _start(SB), $0
    MOVW    CPSR, R0
    MOVW    R0, CPSR
    MOVW    $0xd3, CPSR
    MOVW    CPSR, R1
    MOVW    R1, R0
    MOVW    $1, R7
    SWI     $0
