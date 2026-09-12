// Atomic load-linked/store-conditional (case 48, LL; case 47, SC).
// ZOREG (offset==0) only: unlike case 7/8, goken's optab.c never
// declares a large-offset/entity variant for these at all, so
// there's nothing to port beyond the single-instruction form (see
// Codegenv.ml's comment). SC (store) has no delay slot -- fine
// chained with anything. LL (load) has the same mandatory 1-NOP
// load-delay-slot hazard as case 8/27/36, so it's kept right before
// SYSCALL here (nothing eligible to hoist into the slot, confirmed
// via `vl -a`) to stay byte-identical -- see atomic_case47_48_check.s
// for a version that actually reads the loaded value back, which
// gives the scheduler something to hoist and isn't byte-identical.
// Uses the stack pointer (valid memory) so both sides actually run
// instead of segfaulting on a garbage address.
TEXT _start(SB), $0
    MOVW    $42, R2
    SC      R2, 0(R29)
    LL      0(R29), R1
    SYSCALL
