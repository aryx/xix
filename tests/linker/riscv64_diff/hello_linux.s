// RISC-V64 Linux hello world.
//
// Identical source to tests/linker/riscv_diff/hello_linux.s -- goken's
// ja/jl are literally the same binaries as ia/il (thechar dispatches on
// argv0 at runtime, see mkfiles/riscv64/mkfile), and none of xix's
// Ast_asmi/Parse_asmi/Rewritei/Layouti/Codegeni reference Arch.t at all,
// so the same assembly source and the same OCaml code paths produce this
// binary too. Kept as its own file (rather than reused across dirs) to
// mirror the one-fixture-per-arch-dir convention of arm_diff/mips_diff/
// riscv_diff.
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
