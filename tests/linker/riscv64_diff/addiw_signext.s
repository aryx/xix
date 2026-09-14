// "ADDW $I,[R,]D" (ADDIW, riscv64-only) -- a real, confirmed bug
// found while writing this: the 6th instance of this whole effort's
// running "goken's own -S output isn't valid re-assembleable input
// to itself" bug family. The compiled Prog's own immediate is a
// small, valid ADDIW value (e.g. -1), but goken's own `ic`'s Pconv
// prints it as its unsigned 32-bit representation
// ("$4294967295") instead of the signed one ("$-1") -- real goken's
// own `ja`/`jl` then REJECT that exact spelling outright ("illegal
// combination", confirmed empirically), since ADDIW's own operand
// class check operates on the raw literal value, not a 32-bit-
// truncated-then-sign-extended reinterpretation of it. No goken
// reference exists for this exact literal spelling either way, so
// this fixture checks both spellings against EACH OTHER (not
// against goken): "$4294967295" and "$-1" must produce identical
// bytes, since both represent the same real ADDIW immediate once
// correctly reinterpreted. NOT wired into test-riscv64.sh's own
// CASES array -- diff-riscv64.sh's own harness assumes real goken
// can assemble every fixture it's given, which isn't true here;
// verified manually instead (`cmp` against the "$-1" spelling's own
// output, plus a qemu run checking the expected exit code), and by
// the real closure (tests/linker/hello_libc_riscv64/) exercising the
// exact same real construct end to end.
TEXT _start(SB), $0
    MOVW $10, R11
    ADDW $4294967295, R11, R12   // == ADDW $-1,R11,R12 -- R12 = 9
    MOVW R12, R10
    MOVW $93, R17
    ECALL
