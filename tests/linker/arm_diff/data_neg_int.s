// A negative integer in a DATA statement -- not necessarily a real
// negative *number*, e.g. real 5c -S output for fmt/nan64.c's own
// "uvneginf<>" (a raw IEEE754 -Inf bit pattern, sign bit set) uses
// this same Int DATA path for what's really just a bit pattern.
// Real, valid 5a syntax (confirmed against goken's real 5a).
// fill_bytes_for_int previously rejected any negative value outright
// (a pre-existing TODO already flagged this: "if negative still need
// check range and convert to corresponding unsigned value"). Found
// stress-testing against real lib_core/libc -- see
// docs/claude_notes/plan_hello_libc_linking.md.
//
// tab<> holds -1 (0xffffffff); loading and exiting with it should
// give exit code 255 (only the low byte of R0 survives as the exit
// status).

TEXT _start(SB), $0
	MOVW	$tab<>+0(SB),R1
	MOVW	0(R1),R0
	MOVW	$1,R7
	SWI	$0

GLOBL	tab<>+0(SB), $4
DATA	tab<>+0(SB)/4,$-1
