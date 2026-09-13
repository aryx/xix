// -------------------------------------------
// main procedure
// -------------------------------------------

//old: 0 below was '7' before, to encode NOPROF | DUPOK | NOSPLIT
// and NOSPLIT was what mattered to tell 6l to not do go-specific stack
// stuff
//update: I have now added some ifdef GOLANG in 6l so don't need it anymore
//we use _start and not main to avoid triggering Go-specific stuff in 6l
//update: can also now use 6l -X to disable the go-specific stuff
// (but now most stuff is disabled by default thx to #ifdef GOLANG)
TEXT    _start+0(SB), 0, $0

        // Allocate space for return address (CALL pushes return addr)
        SUBQ    $16, SP         // make space: 8 bytes each for buf, len

        LEAQ    msg(SB), AX     // get pointer to message
        MOVQ    AX, 0(SP)       // push buf (arg 1)
        MOVQ    $13, 8(SP)      // push len (arg 2)

        CALL    write(SB)

        ADDQ    $16, SP         // clean up stack

        // syscall: exit(0)
        MOVQ    $60, AX
        XORQ    DI, DI
        SYSCALL

// -------------------------------------------
// write(buf *byte, len int)
// -------------------------------------------
TEXT    write+0(SB), $0

        MOVQ    $1, AX          // syscall: write
        MOVQ    $1, DI          // fd = 1 (stdout)

        MOVQ    buf+0(FP), SI   // arg 1: buf
        MOVQ    len+8(FP), DX   // arg 2: len

        SYSCALL
        RET


// -------------------------------------------
// msg: must split into 8-byte chunks
// -------------------------------------------
DATA    msg+0(SB)/8, $"Hello, w"
DATA    msg+8(SB)/5, $"orld\n"
GLOBL   msg(SB), $13
