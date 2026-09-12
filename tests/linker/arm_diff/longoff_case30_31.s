// Long (>12-bit) stack-frame offset load/store (cases 30/31): the
// offset doesn't fit STR/LDR's 12-bit immediate, so goken must load
// it into REGTMP via the literal pool first, then use a
// register-offset LDR/STR. Each large offset value here is used
// exactly once (store at 5000, load at 6000) to avoid goken's
// literal-pool value deduplication (a separate, undocumented gap in
// our Layout5.ml -- see arm_port.md), which would
// otherwise make the pool layout diverge for a reason unrelated to
// case 30/31's actual codegen.
TEXT _start(SB), $8192
    MOVW    $0x2a, R0
    MOVW    R0, 5000(R13)
    MOVW    6000(R13), R1
    MOVW    R1, R0
    MOVW    $1, R7
    SWI     $0
