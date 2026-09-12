// Minimal exit(42), no data section -- the same starting fixture used
// for every other arch's differential-testing harness (see
// docs/claude_notes/arm_port.md's "Test corpus sources" for why a
// no-data-section fixture matters on its own, catching ELF
// string-table-size bugs a data-having fixture can't).
//
// AArch64 Linux syscall convention: syscall number in R8, args in
// R0-R5, "SVC $0".
TEXT _start(SB), $0
    MOV $42, R0
    MOV $93, R8
    SVC $0
