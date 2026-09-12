// 64-bit long multiply (case 17): all four sign/accumulate variants
// (MULL/MULLU/MULAL/MULALU), each computing 100000*100001 into a
// register pair -- exercises gmull_opcode's sign/accumulate bit
// packing precisely enough to catch the two bits being swapped (a
// real bug caught while writing this fixture: MULL's own encoding
// came out identical to MULALU's, confirmed against goken directly).
// $100000/$100001 (not the same constant twice) to avoid the
// already-known literal-pool-deduplication gap, same reasoning as
// longoff_case30_31.s. MULAL/MULALU accumulate into (R5,R6),
// pre-zeroed, so their result is identical to MULL/MULLU's for this
// check.
TEXT _start(SB), $0
    MOVW    $100000, R1
    MOVW    $100001, R2
    MOVW    $0, R5
    MOVW    $0, R6

    MULL    R1, R2, (R3, R4)   // hi=2 (100000*100001 = 0x2_540DC020)
    MULLU   R1, R2, (R7, R8)
    MULAL   R1, R2, (R5, R6)
    MOVW    $0, R9
    MOVW    $0, R10
    MULALU  R1, R2, (R9, R10)

    ADD     R3, R7, R0
    ADD     R0, R5, R0
    ADD     R0, R9, R0
    MOVW    $1, R7
    SWI     $0
