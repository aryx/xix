// xexit_arm.s — Linux ARM32 exit(0) using Plan 9 5a syntax

TEXT    xexit+0(SB), $0
        MOVW    $0, R0          // status = 0 (argument)
        MOVW    $1, R7          // syscall number 1 = sys_exit
        SWI     $0              // trap into kernel
	RET // never reached
