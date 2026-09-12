// case 5: "jalr D,I(S)" / "jmp I(S)" -- indirect jump through a
// register plus a signed offset, goken's `OP_I(classreg(to), r, v)`.
// A genuinely different operand shape from case 4's JAL/JALR (which
// targets a label), even though goken's own grammar aliases "JAL"/
// "JALR"/"JMP" onto operand-shape dispatch rather than distinct
// mnemonics -- see Ast_asmi.ml's JALRI comment. `inc` returns via the
// JMP-spelled form (rd defaults to REGZERO, a true jump discarding
// the return address); `double` returns via the explicit-D JALR-
// spelled form (rd=R9, an ordinary register, just never read again).
//
// Deliberately avoids "$proc(SB)" (address-of-procedure) to reach
// either callee -- reuses the already-verified case 4 JAL mechanism
// for that instead. A real gap was found taking that route on RV64
// (address-of-procedure uses AUIPC there in goken, this port still
// used an absolute LUI+ADDI, functionally correct but not byte-
// identical) -- separate from case 5 itself, not fixed here, see
// riscv_port.md.
TEXT inc(SB), $0
    ADD $1, R6, R6
    JMP 0(R1)

TEXT double(SB), $0
    ADD R6, R6, R6
    JALR R9, 0(R1)

TEXT _start(SB), $0
    MOVW $10, R6
    JAL R1, inc(SB)
    JAL R1, double(SB)
    ADD $0, R6, R10
    MOVW $93, R17
    ECALL
