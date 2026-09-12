// mov r,soreg (case 7), int register-indirect store. Splits into
// ZOREG (offset==0, single instruction) and LOREG (any other
// offset, REGTMP-based 4-instruction expansion, same shape as case
// 35/36's Entity variant) -- goken's C_SOREG fast path is
// permanently dead on MIPS (BIG=0), just like C_SECON/C_SACON for
// address-of; only offset==0 (C_ZOREG) actually reaches the
// single-instruction path. Confirmed via `vl -a` directly. Stores
// have no load-delay-slot hazard (unlike case 8's loads -- see
// mem_move_case7_8_check.s for a load-based functional check that
// accepts that separate, already-documented scheduler gap), so this
// stays byte-identical. Writes below the current stack pointer
// (still valid, unused stack memory) to avoid a segfault under
// qemu-mips.
TEXT _start(SB), $0
    MOVW    $42, R1
    MOVW    R1, -8(R29)
    MOVW    R1, -4(R29)
    MOVW    $0, R4
    MOVW    $4001, R2
    SYSCALL
