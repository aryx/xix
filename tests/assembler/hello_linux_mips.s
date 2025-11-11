// Registers in Plan 9 syntax correspond roughly as:
// R1 → $at
// R2 → $v0
// R3–R10 → $a0–$a7

// -------------------------------------------
// main procedure
// -------------------------------------------
TEXT _start(SB), $0

	// optional MOVW if set BIG to 0 in l.h in which case $msg(SB)
        // further below will have the same value then $setR30(SB) and
        // will not need SB (R12) to be set first.
	MOVW	$setR30(SB), R30

        /* write(1, msg, len) */
        MOVW    $1, R4              /* fd = 1 (stdout)          */
        MOVW    $msg(SB), R5        /* buf = &msg               */
        MOVW    $13, R6             /* count = len              */
        MOVW    $4004, R2           /* syscall = write          */
        SYSCALL

        /* exit(0) */
	MOVW    $0, R4              /* exit code                */
        MOVW    $4001, R2           /* syscall = exit           */
        SYSCALL

// test case for nm/onm
TEXT never_executed(SB), $0
	//JAL unknown_function(SB)
	JAL never_executed(SB)
	RET

// -------------------------------------------
// data section
// -------------------------------------------
GLOBL   msg(SB), $13
DATA    msg+0(SB)/8, $"Hello, w"
DATA    msg+8(SB)/5, $"orld\n"
