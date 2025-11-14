TEXT _start+0(SB), $0

    // RISC-V: a0 = arg0, a7 = syscall number
    MOVW $42, R0      // exit code
    MOVW $93, R7      // syscall number = exit (93)
    ECALL             // trap to kernel
    RET               // not reached
