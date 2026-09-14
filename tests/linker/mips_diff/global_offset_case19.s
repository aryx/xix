// Exercises "MOVW $sym+N(SB),Rt" (address-of-global-plus-offset) with
// a genuinely nonzero N -- a real, confirmed bug this session found
// and fixed in Codegenv.ml's case 19 (the offset field, previously
// named `_offsetTODO`, was silently discarded, always computing
// sym+0 regardless of N; same already-documented bug shape ARM32's
// own port hit and fixed, see arm_port.md's hello_libc section, just
// never ported to MIPS until this closure stress test found it too).
// kitchen_sink.s's own two-GLOBL setup does NOT exercise this --
// each of its symbols is addressed at offset 0 from itself. This
// fixture instead mirrors real 7c's own convention of packing
// multiple string literals into ONE shared blob per C file, each
// addressed at a different nonzero byte offset from the SAME symbol
// -- exactly the shape that broke hello.c's own real "%d" formatting
// (fmt/dofmt.c's shared digit-table string) until this fix.
TEXT _start(SB), $0
    MOVW $setR30(SB), R30
    MOVW $1, R4
    MOVW $blob+4(SB), R5
    MOVW $6, R6
    MOVW $4004, R2
    SYSCALL
    MOVW $0, R4
    MOVW $4001, R2
    SYSCALL
GLOBL blob(SB), $10
DATA blob+0(SB)/4, $"hi, "
DATA blob+4(SB)/6, $"world\n"
