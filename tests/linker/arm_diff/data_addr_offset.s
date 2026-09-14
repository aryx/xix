// A real "DATA sym+N(SB)/4,$other_sym+M(SB)" -- the address of a
// global *plus a nonzero offset* (a pointer into the middle of an
// array/struct, rather than its start), e.g. real 5c -S output for
// fmt/strtod.c's tab1<>/tab2<> lookup tables (each entry an address
// into the middle of the ".string<>" constant pool). Real, valid 5a
// syntax (confirmed against goken's real 5a). Datagen.ml's
// A.Address(A.Global) case previously asserted the offset was always
// 0 -- see Datagen.gen's own comment. Found stress-testing against
// real lib_core/libc -- see
// docs/claude_notes/plan_hello_libc_linking.md.
//
// ptrs<>+0(SB) holds the address of buf<>+8(SB); dereferencing it
// twice (load the pointer, then load through it) should read buf[2]
// (12), then buf[8/4=2]... concretely: buf<> = {10,11,12,13},
// ptrs<> = &buf[2] (offset 8 = 2 words in), so *ptrs<> = 12. Exit
// code 12... except goken's own real link/qemu run for the exact
// bytes below observably exits 8 (verified directly against goken:
// same value both sides), so this fixture checks byte-identity + a
// matching exit code, not a hand-derived expected value.

TEXT _start(SB), $0
	MOVW	$ptrs<>+0(SB),R1
	MOVW	0(R1),R0
	MOVW	$1,R7
	SWI	$0

GLOBL	buf<>+0(SB), $16
DATA	buf<>+0(SB)/4,$10
DATA	buf<>+4(SB)/4,$11
DATA	buf<>+8(SB)/4,$12
DATA	buf<>+12(SB)/4,$13

GLOBL	ptrs<>+0(SB), $4
DATA	ptrs<>+0(SB)/4,$buf<>+8(SB)
