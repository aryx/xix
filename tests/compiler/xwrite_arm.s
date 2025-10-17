// xwrite_arm.s — Linux ARM32 write(fd=1, buf, count)

TEXT xwrite+0(SB), $0
#ifdef arm_
	//with 5c, all args are passed in the stack (use 5c -S on helloc.c)
	//with 5c_, the first arg is passed via R0
	MOVW    R0, R1
#else
        MOVW    buf+0(FP), R1   // buf pointer (first arg)
#endif

        MOVW    count+4(FP), R2 // count (second arg)
        MOVW    $1, R0          // fd = 1 (stdout)
        MOVW    $4, R7          // syscall number 4 = sys_write
        SWI     $0              // trap into kernel
	RET

