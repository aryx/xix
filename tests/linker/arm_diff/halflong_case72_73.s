// Long (>8-bit) stack-frame offset halfword store/load (cases 72/73,
// the V4 real-instruction long-offset forms -- mirrors cases 30/31
// for plain word/byte, just using ghalfword's register-offset form
// instead of gmem's). Each offset used exactly once (5000, 6000,
// 7000) to avoid goken's literal-pool value deduplication (see
// tests/linker/arm_diff/longoff_case30_31.s and
// docs/claude_notes/todo_arm_port.org).
TEXT _start(SB), $8192
    MOVW    $0x1234, R1
    MOVH    R1, 5000(R13)
    MOVH    6000(R13), R2
    MOVB    7000(R13), R3
    MOVW    R2, R0
    MOVW    $1, R7
    SWI     $0
