// Long (>10-bit-word-count) stack-frame offset float store/load
// (case 52/53, FPA -- the REGTMP-via-literal-pool fallback, since
// FPA/VFP load/store has no register-offset addressing mode at all
// unlike int LDR/STR): mirrors longoff_case30_31.s for plain
// word/byte, just for float. Store and load use two *different*
// large offsets (4096 vs 8192), same reason as longoff_case30_31.s:
// goken's literal-pool value deduplication (addpool() reuses an
// existing constant, see the "literal-pool value deduplication" TODO
// near the top of arm_port.md) would otherwise make the pool
// layout diverge for a reason unrelated to case 52/53's own codegen
// -- confirmed by hitting exactly that 2-byte diff with a same-
// offset round-trip version of this fixture before rewriting it this
// way. Not a round-trip of a known value (the loaded float comes
// from fresh, unspecified stack memory), same reasoning as
// longoff_case30_31.s's own int load: byte-identical guarantees an
// identical (if not independently meaningful) exit code on both
// sides.
TEXT _start(SB), $8200
    MOVW    $42, R0
    MOVWF   R0, F0
    MOVF    F0, 4096(R13)
    MOVF    8192(R13), F1
    MOVFW   F1, R1
    MOVW    R1, R0
    MOVW    $1, R7
    SWI     $0
