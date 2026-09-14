// The riscv64-only explicit-32-bit-view "*W" opcode family --
// ADDW/SUBW/SLLW/SRLW/SRAW/MULW/DIVW/DIVUW/REMW/REMUW (register-
// register), and ADDIW/SLLIW/SRAIW (immediate, sharing the same
// mnemonic tokens as their register-register siblings, dispatched by
// operand shape same as plain ADD/SLL/SRA already are). Real RISC-V's
// own OOP_32/OOP_IMM_32 major opcodes (0x3b/0x1b) instead of OOP/
// OOP_IMM's 0x33/0x13 -- otherwise identical funct3/funct7 to their
// native-width siblings (confirmed against real goken's own
// optab.c). Previously completely unimplemented (`oprrr_arith_opcode`
// had a standing `failwith "TODO:...RV64 *W ops"` for every one of
// them). Found stress-testing real lib_core/libc on riscv64
// specifically (e.g. fmt/dofmt.c's own real "SRAW R15,R9,R12").
//
// Also covers a real, confirmed bug found while writing this: SUBW
// has no immediate opcode any more than plain SUB does -- goken's
// own linker (linkers/il/obj.c, the SAME case ASUB/ASUBW switch arm)
// rewrites a constant-operand SUBW into ADDW with the immediate
// negated, mirrored here as the exact same rewrite as plain SUB's
// own (see Codegeni.ml's own `resolve_entities`).
TEXT _start(SB), $0
    MOVW $23, R8
    MOVW $5, R9
    ADDW R9, R8, R10        // R10 = 28
    SUBW $3, R10, R10       // R10 = 25 (SUBW-immediate -> ADDW $-3 rewrite)
    SLLW $2, R9, R11        // R11 = 20
    SRLW $1, R11, R11       // R11 = 10
    ADDW R11, R10, R10      // R10 = 35
    MULW R9, R9, R12        // R12 = 25
    DIVW R9, R12, R13       // R13 = 5
    ADDW R13, R10, R10      // R10 = 40
    REMW R9, R8, R14        // R14 = 23 % 5 = 3
    ADDW R14, R10, R10      // R10 = 43
    ADDW $5, R10, R10       // ADDIW fast path -- R10 = 48
    MOVW $93, R17
    ECALL
