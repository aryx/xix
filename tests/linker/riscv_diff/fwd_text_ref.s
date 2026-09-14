// "MOV $later_func(SB),R" -- taking the address of a TEXT symbol
// defined *later* in the same link (a real forward reference, e.g.
// fmt/fmtfd.c's own fmtfdinit takes the address of fmt/fmtfdflush.c's
// __fmtFdFlush, defined in a later unit of the same link). A real,
// confirmed bug: `env.syms`'s own TEXT entries are registered
// *incrementally* by Layouti.layout_text as it walks the code graph
// forward (a symbol's own real_pc isn't knowable until every
// instruction before it has already been sized) -- a lookup for a
// not-yet-visited TEXT symbol used to crash outright (`Hashtbl.find`
// raising `Not_found`), rather than deferring the actual value lookup
// into the binary-emission thunk (which runs after layout has fully
// completed, by which point every TEXT symbol is registered) the way
// the SData2 slow path's own `init_data` lookup already did.
TEXT _start(SB), $0
    MOVW $setSB(SB), R3
    MOV $later(SB), R8
    JALR R1, 0(R8)
    MOVW R10, R10
    MOVW $93, R17
    ECALL
TEXT later(SB), $0
    MOVW $77, R10
    JMP 0(R1)
