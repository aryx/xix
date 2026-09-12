// BL (call) into a leaf procedure, using the compiler-facing "RETURN"
// pseudo-op (not the raw hardware "RET") so Rewrite7.ml's leaf/frame-
// size-driven prologue+epilogue synthesis kicks in -- mirrors goken's
// own linkers/7l/noop.c. _start itself becomes non-leaf (it calls
// BL), even though it has no RETURN of its own (it exits via SVC
// directly) -- confirmed against goken that this still gets its own
// (unused, since _start never returns) link-register-save prologue.
TEXT _start(SB), $0
    MOV $10, R1
    BL square(SB)
    MOV $93, R8
    SVC $0

// square(R1) -> R0 = R1*R1, a true leaf ($0 frame, no calls) -- gets
// no prologue/epilogue at all (case 1).
TEXT square(SB), $0
    MUL R1, R1, R0
    RETURN
