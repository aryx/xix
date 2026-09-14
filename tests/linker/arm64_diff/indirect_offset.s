// Exercises the plain register-base Indirect load/store's own 3-tier
// offset handling added this session in Codegen7.ml (scaled-12-bit /
// unscaled-9-bit / gindirect_huge fallback) -- sized_move.s already
// covers the plain scaled-aligned-nonnegative case (offset >= 0,
// divides evenly by the access size); this fixture is specifically
// the negative-but-in-range (unscaled-9-bit, -256..255) and the
// genuinely huge (beyond that, needs REGTMP+ADD to materialize the
// real address) forms. Found stress-testing real lib_core/libc
// (fmt/dofmt.c's real "MOVW -8(R3),R3": a negative Indirect offset
// the scaled-12-bit-only fast path used to reject outright).
//
// The huge-tier lines are NOT expected to match goken functionally
// either (only xix's own exit code matters here): real goken's own
// omovlit-based fallback for a negative register-relative Indirect
// offset (its own "add -N (...)" diagnostic at assemble time)
// reliably SIGSEGVs at runtime for every negative huge offset tried
// (-260, -300, -1000, all reproduced directly against real goken/
// qemu-aarch64) -- a confirmed real goken bug, not a xix mistake,
// and exactly why gindirect_huge is a genuinely different (working)
// xix-only mechanism rather than a port of goken's own.
TEXT _start(SB), $0
    MOV $setSB(SB), R28
    MOV $buf(SB), R5
    ADD $1000, R5, R5       // R5 now points 1000 bytes into buf
    MOV $7, R1
    MOVW R1, -8(R5)          // unscaled-9-bit tier (negative, in -256..255)
    MOVW -8(R5), R2          // R2 = 7
    MOV $35, R3
    MOVW R3, -1000(R5)       // gindirect_huge tier (offset < -256) -> writes to buf+0
    MOVW -1000(R5), R4       // R4 = 35, same huge tier for the load
    ADDW R2, R4, R0          // R0 = 7 + 35 = 42
    MOV $93, R8
    SVC $0
DATA buf+0(SB)/8, $0
GLOBL buf(SB), $2008
