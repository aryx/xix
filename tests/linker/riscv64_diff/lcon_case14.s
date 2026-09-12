// case 14: "op $lcon,[r,]d" -- a large immediate (doesn't fit ADDI's
// 12-bit field) for ADD/AND/OR/XOR specifically. goken's optab.c only
// has a C_LCON row for these 4 (confirmed: no SUB/SLT/SLTU/SLL/SRL/
// SRA row exists) -- `OP_RO(r,REGTMP,rt)`'s own encoding has no
// funct7 term at all (always 0), which is exactly why only these 4
// (whose register-register funct7 is already 0) are reachable this
// way; SUB/SRA's own funct7=0x20 genuinely can't be expressed here.
// Materializes the constant into REGTMP via LUI+ADDI (same mechanism
// as case 9/20's own big-constant handling), then does the real op
// register-register using REGTMP as the "from" operand.
TEXT _start(SB), $0
    MOVW $10, R6
    ADD $100000, R6, R7
    AND $0xFFFFF, R7, R8
    OR $100000, R8, R9
    XOR $100000, R9, R10
    MOVW $93, R17
    ECALL
