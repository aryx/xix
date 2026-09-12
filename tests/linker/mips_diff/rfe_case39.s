// RFE (case 39): kernel-only "return from exception", JR (r)
// followed by a fixed RFE instruction that fills JR's own mandatory
// delay slot by design (no extra nop needed -- see Codegenv.ml's
// comment). Nothing follows it: unlike every other fixture this
// session, goken's own assembler treats code after an RFE (an
// unconditional computed jump with no following label) as dead and
// drops it entirely -- confirmed empirically (a trailing SYSCALL
// vanished from goken's output bytes, though not from xix's, since
// this port doesn't implement that dead-code elimination). Not
// chased further, real Plan9 kernel code puts RFE last in an
// exception handler anyway. Byte-comparison only: RFE is
// privileged, illegal under qemu-mips user-mode emulation, so this
// isn't runnable as a meaningful functional check (goken's own
// output would trap identically, same as cop0_move_case37_38.s's
// MTC0/MFC0).
TEXT _start(SB), $0
    RFE     (R5)
