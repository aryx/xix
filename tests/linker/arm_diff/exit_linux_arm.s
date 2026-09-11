// -------------------------------------------
// main procedure
// -------------------------------------------

TEXT _start+0(SB), $20

        MOVW $42, R0 /* exit code */
        MOVW $1 , R7 /* syscall number = exit */
        /* system call */
        SWI $0
        RET /* not reached */
