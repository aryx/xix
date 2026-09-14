// Exercises "FMOVD $con,Fd" routed through the literal pool with a
// constant whose raw IEEE754 bit pattern has nonzero bits in BOTH
// the low and high 32-bit halves -- this session's fix to
// Codegen.ml's WORD case (which used to raise Todo for any
// Ast_asm.Float entirely) and Layout7.ml's own high-word splicing
// (which used to hardcode the high word to 0 for every float,
// silently truncating any double whose upper half wasn't zero). A
// wrong split shows up directly in the FCVTZSD result below. Found
// stress-testing real lib_core/libc (fmt/strtod.c's own real
// "FMOVD $4.29496729500000000e+09,F3").
//
// Also not expected to match goken functionally (only xix's own exit
// code matters): real goken's own chipfloat-immediate mechanism is
// already documented as dead/broken for anything outside its own 8
// magic constants (see bitcon64_params/ARM32's own chipfloat
// history), and reliably SIGSEGVs at runtime for this constant,
// confirmed directly against real goken/qemu-aarch64.
TEXT _start(SB), $0
    FMOVD $4294967295.0, F1
    FCVTZSD F1, R0          // R0 = 4294967295 = 0xFFFFFFFF -> exit code 255
    MOV $93, R8
    SVC $0
