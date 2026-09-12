// Control-and-status register access (case 22): CSRRW/CSRRS/CSRRC.
// "CSR" is a separate keyword from the mnemonic itself (mirrors
// goken's own lexer, LCTL vs LCSR), so the concrete syntax is
// "CSRRW CSR($num),S,D". Not a general-purpose test since actual
// CSR numbers require privileged mode to be meaningful on real
// hardware/qemu-user (which only emulates user-mode instructions),
// so this only checks the encoding (byte-identical against goken),
// not the runtime CSR read/write effect itself -- same reasoning as
// why ECALL's own encoding-only tests never inspect kernel state.
TEXT _start(SB), $0
    MOVW    $0x1234, R5
    CSRRW   CSR(0x300), R5, R6
    CSRRS   CSR(0x301), R7, R8
    CSRRC   CSR(0x302), R9, R10

    MOVW    $93, R17
    MOVW    $42, R10
    ECALL
