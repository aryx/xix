// Real RISC-V M-extension (MUL/DIV/DIVU/REM/REMU) -- previously
// completely unimplemented (ArithMul had AST/grammar scaffolding but
// no Codegeni.ml case at all). Standard funct7=0x01 marker with
// funct3 selecting the operation (0=MUL,4=DIV,5=DIVU,6=REM,7=REMU),
// same `middle`/`from`/`to` -> rs1/rs2/rd mapping as plain Arith
// (confirmed by hand-decoding real goken bytes for "DIV R5,R6,R7"),
// genuinely worth checking separately from ADD/SUB since DIV/REM
// aren't commutative -- a left-right swap here would silently
// compute the wrong *value*, not just wrong bytes for an equivalent
// one. Both the 3-register and 2-register in-place forms are
// checked. Found stress-testing real lib_core/libc (port/vlrt.c's
// own 64-bit-division helpers, and fmt/dofmt.c's own "%d" digit-
// extraction loop, "REMU R12,R11" / "DIVU R13,R9").
TEXT _start(SB), $0
    MOVW $23, R8
    MOVW $5, R9
    DIV R9, R8, R10         // R10 = 23/5 = 4
    REM R9, R8, R11         // R11 = 23%5 = 3
    ADD R11, R10, R10       // R10 = 7
    MOVW $100, R12
    MOVW $9, R13
    DIVU R13, R12, R14      // R14 = 100/9 = 11
    ADD R14, R10, R10       // R10 = 18
    REMU R9, R8             // 2-op: R8 = R8 % R9 = 23%5 = 3
    ADD R8, R10, R10        // R10 = 21
    MOVW R10, R10
    MOVW $93, R17
    ECALL
