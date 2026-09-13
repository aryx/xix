// Exercises the remaining SSE float instructions found in real
// 6c-compiled output (see plan_amd64_port.md's "Real bugs/quirks"):
// CVTSL2SD/CVTSL2SS/CVTTSD2SL/CVTTSS2SL (the 32-bit-int forms of the
// int<->float conversions -- same shape as the 64-bit forms already
// wired, just no REX.W), CVTSD2SS/CVTSS2SD (float precision
// conversion, one real opcode both directions, disambiguated by
// prefix), and XORPD's own self-clear idiom (mirroring the existing
// GP-register Zclr special case -- confirmed real, used directly in
// 6c-compiled float negation: "XORPD X0,X0" then "SUBSD ...").
//
// Self-check: 13(int32)->13.0, 4(int32)->4.0f->4.0(double), summed to
// 17.0; XORPD self-clears an XMM register to 0.0 (checked by negating
// and re-clearing before reusing it), 17.0->17.0f->17 (int32,
// truncating); a separate 9->9.0->9 round trip through R8-R15. 17+9 =
// 26, the final exit code.
TEXT _start(SB), $0
	MOVL	$13, AX
	CVTSL2SD	AX, X0      // X0 = 13.0 (32-bit int source, no REX.W)
	MOVL	$4, BX
	CVTSL2SS	BX, X1      // X1 = 4.0f (single precision)

	CVTSS2SD	X1, X2      // X2 = 4.0 (single -> double)
	ADDSD	X2, X0              // X0 = 17.0

	XORPD	X3, X3               // X3 = 0.0
	SUBSD	X0, X3               // X3 = 0.0 - 17.0 = -17.0
	XORPD	X3, X3               // clear again -- X3 = 0.0 (self-clear works)
	ADDSD	X0, X3               // X3 = 17.0

	CVTSD2SS	X3, X4      // X4 = 17.0f (double -> single)
	CVTTSS2SL	X4, CX      // CX = 17 (32-bit int result, no REX.W)

	MOVQ	$9, R9
	CVTSL2SD	R9, X5      // exercise R8-R15 as a 32-bit int source
	CVTTSD2SL	X5, R10     // R10 = 9 (R8-R15 as a 32-bit int dest)

	ADDL	R10, CX             // CX = 17 + 9 = 26
	MOVQ	CX, DI              // exit(26)
	MOVQ	$60, AX
	SYSCALL
