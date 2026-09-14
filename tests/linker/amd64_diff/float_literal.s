// Real amd64 has no opcode to move/combine an immediate float directly
// into an XMM register -- goken's real linkers/6l/obj.c (AMOVSD/
// AMULSD/etc.'s own D_FCONST preprocessing case) handles a literal
// float source by synthesizing a hidden DATA symbol (named by the
// float's own IEEE754 bit pattern) and rewriting the instruction to
// reference it instead -- an auto-generated literal pool. This port
// mirrors that in Rewrite6.ml. Confirmed against real fmt/fltfmt.c's
// own "MOVSD $(1.0e+00),X0" / "MULSD $(3.0e-01),X0". Deliberately
// uses only one distinct literal value (referenced twice) rather than
// several: real 6l's own internal symbol-table ordering for *multiple*
// synthesized constants isn't something this port's own Rewrite6.ml
// tries to replicate (only each constant's own value and the
// program's own observable behavior), so a fixture with more than one
// distinct literal can legitimately end up with them in a different
// (but equally correct) order in the data segment.
TEXT	_start(SB), $0
	MOVSD	$(2.00000000000000000e+00),X0
	MOVSD	$(2.00000000000000000e+00),X1  // same literal, reused symbol
	ADDSD	X1,X0                           // X0 = 4.0
	MULSD	$(2.00000000000000000e+00),X0   // X0 = 8.0
	CVTTSD2SL	X0,AX                       // AX = 8
	MOVQ	AX,DI
	MOVQ	$60,AX
	SYSCALL
