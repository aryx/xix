TEXT exit(SB), $4
        /* prepare the system call EXITS(0) */
        MOVW $0, R1
        MOVW R1, 4(R13)
        MOVW $3 /*EXITS*/, R0
        /* system call */
        SWI $0
        RET /* not reached */
        

GLOBL   hello(SB), $12
DATA    hello+0(SB)/6, $"Hello "

