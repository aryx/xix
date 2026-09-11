// ADD with an immediate at the boundaries of case 4 (direct fit in
// ADDI/ADDIU's native sign-extended 16-bit immediate, actual class
// ZCON/SCON/ADDCON: [-0x8000, 0x7fff]) and case 10 (ANDCON, needs a
// REGTMP+OR expansion: [0x8000, 0xffff]). Each pair of adjacent
// boundary values is combined with ADD/SUB (case 2, already
// ported) into a small, unambiguous delta and checked with BEQ --
// a real bug in either case 4's guard or case 10's expansion would
// show up as a wrong exit code under qemu-mips, not just a byte
// diff (case 4's old code silently truncated any out-of-range
// immediate via `land 0xffff` instead of guarding/expanding it).
//
// Deliberately NOT comparing against a plain `MOVW $-32768, Rx`:
// that exact literal hits an unrelated, pre-existing quirk in
// goken's *own* assembler (case 3's MOVW ends up emitting a
// literal-pool load from an SB-relative symbol instead of a direct
// immediate, and the resulting executable then segfaults under
// qemu-mips on goken's side) -- nothing to do with case 4/10's ADD
// path, which handles -32768 correctly (a single ADDI, confirmed
// via `vl -a` directly). Combining it with another boundary value
// via ADD instead keeps this fixture about case 4/10, not that
// separate MOVW quirk.
TEXT _start(SB), $0
    MOVW    $0, R4

    // case 4 upper edge (32767) vs case 10 lower edge (32768):
    // adjacent, difference must be exactly 1.
    MOVW    $0, R1
    ADD     $32767, R1
    MOVW    $0, R2
    ADD     $32768, R2
    SUB     R1, R2, R3
    MOVW    $1, R5
    BEQ     R3, R5, ok1
    JMP     after1
ok1:
    ADD     $1, R4, R4
after1:

    // case 4 lower edge (-32768) vs case 10 lower edge (32768):
    // must cancel to exactly 0.
    MOVW    $0, R1
    ADD     $-32768, R1
    MOVW    $0, R2
    ADD     $32768, R2
    ADD     R1, R2, R3
    MOVW    $0, R5
    BEQ     R3, R5, ok2
    JMP     after2
ok2:
    ADD     $2, R4, R4
after2:

    // case 10 lower edge (32768) vs upper edge (65535): difference
    // must be exactly 32767.
    MOVW    $0, R1
    ADD     $32768, R1
    MOVW    $0, R2
    ADD     $65535, R2
    SUB     R1, R2, R3
    MOVW    $32767, R5
    BEQ     R3, R5, ok3
    JMP     after3
ok3:
    ADD     $4, R4, R4
after3:

    MOVW    $4001, R2
    SYSCALL
