// Branches (case 3): BEQ/BNE/BLT/BGE/BLTU/BGEU, both the 2-register
// form and the 1-register-vs-zero form, chained into the exit code
// (each taken check adds a distinct power of two: 1,2,4,8,16,32,64
// -> exit code 127 if every check goes the expected way).
//
// Each check uses the *inverted* condition to skip a single
// following instruction (no intervening unconditional JMP)
// deliberately: goken's assembler recognizes the classic "branch
// then immediately jump elsewhere" idiom (BEQ cond,L1; JMP L2) as a
// redundant-jump anti-pattern and rewrites it into an inverted
// branch straight to L2, then *relocates* L1's code to the end of
// the function, threaded back in with its own JMP -- confirmed by
// disassembling goken's actual output for even a single such
// branch+jmp pair. That's a real code-layout optimization in
// goken's own assembler, not something this port replicates (out of
// scope, same category as every other "goken's toolchain does more
// than this port" gap this session) -- so this fixture avoids the
// triggering idiom entirely to stay byte-identical instead of
// documenting yet another _check-style functional-only exception.
//
// goken's own grammar (a.y) rewrites the 1-register branch form
// into from=$zero,reg=<explicit reg> before it ever reaches asm.c's
// case 3 encoding -- see Codegeni.ml's comment on why this ISN'T
// simply "the omitted operand defaults to zero" the way every other
// optional-middle-register case this session works.
TEXT _start(SB), $0
    MOVW    $5, R1
    MOVW    $5, R2
    MOVW    $7, R3
    MOVW    $0, R5
    MOVW    $-1, R6
    MOVW    $0, R10

    // EQ: R1==R2 -> +1
    MOVW    $1, R4
    BNE     R1, R2, skip1
    ADD     R10, R4, R10
skip1:

    // NE: R1!=R3 -> +2
    MOVW    $2, R4
    BEQ     R1, R3, skip2
    ADD     R10, R4, R10
skip2:

    // LT: R5<R1 (0<5) -> +4. Plan9 operand order for branches is
    // "reversed" (like SUB/case2's r2-minus-r1): "OP A,B" compares
    // B against A in hardware, so "is R5<R1" needs "BLT R1,R5"
    // (taken)/"BGE R1,R5" (inverted skip), not the other way round.
    MOVW    $4, R4
    BGE     R1, R5, skip3
    ADD     R10, R4, R10
skip3:

    // GE: R1>=R5 (5>=0) -> +8
    MOVW    $8, R4
    BLT     R5, R1, skip4
    ADD     R10, R4, R10
skip4:

    // LTU: R1 <u R6 (5 <u 0xffffffff) -> +16
    MOVW    $16, R4
    BGEU    R6, R1, skip5
    ADD     R10, R4, R10
skip5:

    // GEU: R6 >=u R1 (0xffffffff >=u 5) -> +32
    MOVW    $32, R4
    BLTU    R1, R6, skip6
    ADD     R10, R4, R10
skip6:

    // 1-register-vs-zero form: R5==0 -> +64
    MOVW    $64, R4
    BNE     R5, skip7
    ADD     R10, R4, R10
skip7:

    MOVW    $93, R17
    ECALL
