// Atomic exchange (case 40): the classic spinlock idiom from goken's
// own runtime (GO/pkg/runtime/arm/cas5.s): "SWPW (R4), R3" swaps R3
// with the word at [R4], leaving the old value in R3.
//
// 0x9000 is deliberately not a real, mapped address -- this fixture
// is only about instruction *encoding* parity (cmp -l), not
// successful execution; both sides segfault identically under
// qemu-arm, which is itself part of the signal (same crash, not a
// different one). This exact constant caught a real, previously
// unnoticed bug in immrot(): goken's own immrot(ulong v) is subtly
// wrong for some values (relative to true ARM rotated-immediate
// semantics) because `ulong` is 64-bit on this host while the
// rotation trick assumes 32-bit wraparound -- see the immrot comment
// in Codegen5.ml. 0x9000 IS a valid ARM immediate (0x90 rotated) but
// goken's own (64-bit-broken) immrot doesn't recognize it, so it
// takes the literal-pool slow path here instead of a single MOV.
TEXT _start(SB), $0
    MOVW    $1, R3
    MOVW    $0x9000, R4
    SWPW    (R4), R3
    MOVW    $1, R7
    SWI     $0
