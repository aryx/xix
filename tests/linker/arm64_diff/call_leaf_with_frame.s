// A leaf procedure with a nonzero frame (a stack-relative local, no
// calls of its own) -- exercises Rewrite7.ml's case 2 (just a "SUB
// $autosize,SP,SP" prologue / "ADD $autosize,SP,SP" epilogue, no
// link-register save/restore at all, since a true leaf never
// clobbers RLINK).
TEXT _start(SB), $0
    MOV $10, R1
    BL addlocal(SB)
    MOV $93, R8
    SVC $0

TEXT addlocal(SB), $16
    MOV R1, 0(RSP)
    MOV 0(RSP), R2
    ADD $90, R2, R0
    RETURN
