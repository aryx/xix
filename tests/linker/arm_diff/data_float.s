// A real float/double constant in the DATA segment, e.g. real 5c -S
// output for fmt/fltfmt.c's "pows10<>" table (goken's own precomputed
// powers-of-ten table for %e/%g float formatting): "DATA
// pows10<>+8(SB)/8,$1.0e+01" etc. Real, valid 5a syntax (confirmed
// against goken's real 5a). Datagen.ml's DATA-segment generator
// previously only handled Int/String/Address values for a DATA
// statement, never Float -- see Datagen.gen's own comment.
// Deliberately includes 1.0e+29 (needs the exponent field's high bit,
// close to the edge of what's representable through this port's
// existing Int64.to_int-based byte-splitting -- see that comment for
// the real caveat this doesn't fully close: a *negative* double would
// still be encoded wrong). See
// docs/claude_notes/plan_hello_libc_linking.md.
//
// Loads tab<>+16(SB) (1.0e+29) as a double and exits 1.

TEXT _start(SB), $0
	MOVW	$tab<>+0(SB),R1
	MOVD	0(R1),F0
	MOVW	$1,R0
	MOVW	$1,R7
	SWI	$0

GLOBL	tab<>+0(SB), $24
DATA	tab<>+0(SB)/8,$1.00000000000000000e+00
DATA	tab<>+8(SB)/8,$1.00000000000000000e+01
DATA	tab<>+16(SB)/8,$1.00000000000000000e+29
