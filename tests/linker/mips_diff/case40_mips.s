// WORD (case 40): a raw constant word embedded directly in the TEXT
// section. Already handled generically by Codegen.default_rules
// (shared across all archs, not MIPS-specific) -- this fixture just
// confirms that generic path produces byte-identical output on
// MIPS too.
TEXT _start(SB), $0
    WORD    $305419896
    MOVW    $0, R4
    MOVW    $4001, R2
    SYSCALL
