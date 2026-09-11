// Functional-shape check for case 38 (MFC0/DMFC0): would read back
// what case 37 just wrote and self-check with BEQ, same as every
// other _check fixture this session. Not run under qemu-mips for a
// real functional verification though: MTC0/MFC0 are privileged
// (kernel-mode) instructions, illegal from qemu-mips user-mode
// emulation on both goken's and xix's output identically (see
// case37_38_mips.s) -- so this is really just a second byte/shape
// confirmation of case 38's encoding and its 2-NOP delay slot,
// structured like a functional check for consistency with the rest
// of this session's fixtures. Not byte-identical for the usual
// reason: chaining case 37 into case 38 (write then read of the
// same M register) plus BEQ/JMP's own gap -- see Codegenv.ml's case
// 38 comment for what's not fully characterized about the nop
// count here.
TEXT _start(SB), $0
    MOVW    $0, R4
    MOVW    $42, R1
    MOVW    R1, M5
    MOVW    M5, R2
    MOVW    $42, R3
    BEQ     R2, R3, ok1
    JMP     after1
ok1:
    ADD     $1, R4, R4
after1:
    MOVW    $4001, R2
    SYSCALL
