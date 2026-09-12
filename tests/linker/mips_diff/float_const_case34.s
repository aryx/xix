// Float-constant load (case 34): reuses case 3's OR-vs-ADDU choice
// (movw_imm_opcode) then MTC1s the result into a float register.
// Unlike case 30/31's standalone MTC1, this does NOT get a trailing
// delay-slot nop (confirmed via `vl -a` -- see Codegenv.ml's case
// 34 comment for what's not fully understood about why). Exercises
// both the ADDCON and ANDCON halves of movw_imm_opcode's choice.
TEXT _start(SB), $0
    MOVW    $42, F0
    MOVW    $32768, F1
    MOVW    $0, R4
    MOVW    $4001, R2
    SYSCALL
