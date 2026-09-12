// Functional check for case 12's sign-extend (MOVB/MOVH): R1 = -1
// (all bits set), truncated to a byte/half and sign-extended back
// should still be exactly -1 either way. Uses BEQ to self-check, so
// (like immcon_case4_10.s/branch_case6.s) this inherits the known
// delay-slot-scheduler byte diff and is deliberately not
// byte-identical -- see docs/claude_notes/todo_mips_port.org.
TEXT _start(SB), $0
    MOVW    $0, R4
    MOVW    $-1, R1
    MOVW    $-1, R5

    MOVB    R1, R2
    BEQ     R2, R5, ok1
    JMP     after1
ok1:
    ADD     $1, R4, R4
after1:

    MOVH    R1, R3
    BEQ     R3, R5, ok2
    JMP     after2
ok2:
    ADD     $2, R4, R4
after2:

    MOVW    $4001, R2
    SYSCALL
