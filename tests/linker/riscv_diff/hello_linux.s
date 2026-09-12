// RISC-V Linux hello world.
//
// The ia assembler maps register names directly (Rn -> xn), and the Linux
// RISC-V syscall ABI is register-specific:
//   a0=R10, a1=R11, a2=R12 -> args;  a7=R17 -> syscall number.
// write=64, exit=93 (the generic Linux syscall numbers, same as arm64).
//
// SB (static base) is REGSB = R3 (x3/gp in the RISC-V ABI); it must be set
// up with setSB so that $msg(SB) resolves, the same way arm64 uses R28 and
// mips uses R30.

TEXT _start(SB), $0

    MOVW    $setSB(SB), R3      // static base (gp), needed for $msg(SB)

    // write(int fd=1, buf=&msg, count=13)
    MOVW    $1, R10             // a0 = fd = 1 (stdout)
    MOVW    $msg(SB), R11       // a1 = buf = &msg
    MOVW    $13, R12            // a2 = count = 13
    MOVW    $64, R17            // a7 = syscall number: write
    ECALL

    // exit(int status=0)
    MOVW    $0, R10             // a0 = status = 0
    MOVW    $93, R17            // a7 = syscall number: exit
    ECALL

// -------------------------------------------
// data section
// -------------------------------------------
DATA    msg+0(SB)/8, $"Hello, w"
DATA    msg+8(SB)/6, $"orld\n\z"
GLOBL   msg(SB), $14
