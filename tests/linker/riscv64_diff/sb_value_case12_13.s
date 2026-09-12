// "MOVW R,sym(SB)" / "MOVW sym(SB),R" through a far offset (case
// 12/13, "mov r,lext"/"mov lext,r") -- the slow-path sibling of
// sb_value_case6_7.s, reached whenever the resolved SB-relative
// offset doesn't fit ADDI's 12-bit field: goken materializes the
// absolute address via LUI/AUIPC into REGTMP (same mechanism as case
// 9/20's own address-of computation) and folds the low 12 bits
// directly into the store/load's own immediate field, rather than a
// separate ADDI. `bigbuf` is deliberately the *only* global in this
// fixture -- combining it with a second, differently-sized global in
// one program was found to trigger a separate, pre-existing
// data-segment layout/alignment discrepancy unrelated to case 12/13
// itself (see todo_riscv_port.org's "Notes for later").
TEXT _start(SB), $0
    MOVW $setSB(SB), R3
    MOVW $77, R6
    MOVW R6, bigbuf+8000(SB)
    MOVW bigbuf+8000(SB), R10
    MOVW $93, R17
    ECALL
DATA bigbuf+0(SB)/4, $0
GLOBL bigbuf(SB), $8192
