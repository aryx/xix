// Address-of-local/param (case 26, MOVW $lsext/auto/oreg,,r2 ==>
// lu+or+add): mirrors ARM's lacon_arm.s. `x-8(FP)` is a Local
// entity, `y+8(SP)` a Param entity (this codebase's own convention:
// the FP token maps to Local, the SP token to Param). Caught a
// real, previously-flagged-as-unverified bug in
// base_and_offset_of_entity: the "+4 for the caller/RLINK-slot
// adjustment" was on Param instead of Local (Local had no
// adjustment at all) -- confirmed against goken directly via
// `vl -a` (frame=$8192: goken computes offset 8192 for x-8(FP) and
// 8204 for y+8(SP), only consistent with autosize=8196 and the +4
// on Local) -- the exact same bug shape as the confirmed ARM one,
// see docs/claude_notes/todo_arm_port.org's case 4/34 entry. No
// small-offset fast path here (goken's C_SACON is permanently dead
// on MIPS, same BIG=0 reasoning as address-of-global -- see
// offset_to_R30's comment), so this always takes the 3-instruction
// LU+OR+ADDU path.
TEXT _start(SB), $8192
    MOVW    $x-8(FP), R1
    MOVW    $y+8(SP), R2
    SUB     R1, R2, R4
    MOVW    $4001, R2
    SYSCALL
