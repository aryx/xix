// Address-of-local/param ($lacon): mirrors the address-of-global
// case 4/12 split (Codegen5.ml), just SP-relative instead of
// SB-relative. Fast path is case 4 (offset fits immrot -> single
// ADD); slow path is case 34 (doesn't fit -> REGTMP via literal
// pool, then ADD).
//
// x-8(FP) and y+8(SP) exercise both directions of this codebase's
// own (inverted-looking, but consistent) convention: Parser_asm5.mly
// maps the FP token to Local and the SP token to Param -- caught two
// real, previously unverified bugs (nothing before this fixture ever
// used a named FP/SP-relative local; every earlier fixture used raw
// Indirect(reg,off) syntax like `4(R13)` instead):
//  - the two entities' respective "+4 for the caller/RLINK-slot
//    adjustment" were on the wrong branches (swapped) in
//    base_and_offset_of_indirect;
//  - with frame=$8192, x-8(FP)'s fully-adjusted offset (8192) fits
//    immrot, but goken still takes the slow/pool path here: its
//    RACON-vs-LACON classification runs on the offset *before*
//    Rewrite5's own "+4 for RLINK-save" adjustment to autosize, a
//    value no longer available by the time Codegen5 runs (see the
//    classification_offset comment in Codegen5.ml).
//
// x-4000(FP) is a second slow-path (case 34) instance, chosen with a
// large enough magnitude that it stays firmly on the LACON side
// regardless of that +4/+8 wobble, so it isolates case 34's actual
// codegen from the classification quirk above.
TEXT _start(SB), $8192
    MOVW    $x-8(FP), R0
    MOVW    $y+8(SP), R2
    MOVW    $x-4000(FP), R1
    MOVW    $1, R7
    SWI     $0
