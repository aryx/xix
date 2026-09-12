// Exercises the byte/halfword/32-bit-view sized Move forms
// (MOVB/MOVBU/MOVH/MOVHU/MOVW/MOVWU) for register<->memory access,
// both through the SB-relative fast path (MOVB R1,buf(SB) / MOVB
// buf(SB),R2) and through a plain register-base Indirect access
// (MOVW R1,0(R5) / MOVWU 0(R5),R7) -- see Codegen7.ml's
// ldstr12u_size_opc/scale_shift_of_size comment for the (sz,opc)
// table and per-size offset scaling this exercises.
//
// Also exercises a real, easy-to-get-wrong gotcha in "MOV $sym(SB),R"
// (address-of-global): unlike a global at data-offset 0 (always
// routed through the literal pool, see global_addr.s), "foo" here
// sits at a nonzero, small offset (following "buf" in the data
// segment) -- goken's own C_AECON classification (span.c's aclass())
// only takes the literal pool when the resolved offset is zero OR
// too big for ADD's addcon immediate; a nonzero, addcon-sized offset
// like this one gets a direct "ADD $offset,RSB,Rt" instead. Caught by
// byte-diffing against goken, not something this port originally
// assumed correctly (see Codegen7.ml's own case-4/case-12 comment).
TEXT _start(SB), $0
    MOV $setSB(SB), R28
    MOV $200, R1
    MOVB R1, buf(SB)
    MOVB buf(SB), R2      // sign-extend load: 0xC8 -> negative 64-bit value
    MOVBU buf(SB), R3     // zero-extend load: 0xC8 -> 200
    MOV $foo(SB), R5      // address-of-global at a nonzero addcon-sized offset -> direct ADD, not the pool
    MOVW R1, 0(R5)
    MOVWU 0(R5), R7       // zero-extend load: 200
    ADD R3, R7, R0        // R0 = 200 + 200 = 400 (mod 256 = 144 as the process exit code)
    MOV $93, R8
    SVC $0
DATA buf+0(SB)/8, $0
GLOBL buf(SB), $8
DATA foo+0(SB)/8, $0
GLOBL foo(SB), $8
