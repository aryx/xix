// Named local variable references against SP ("v+-8(SP)") use
// goken's own "pseudo-SP" convention: the real hardware displacement
// is autosize+offset, not the raw source-level offset -- confirmed
// the hard way stress-testing real lib_core/libc (fmt/vfprint.c's own
// "f+-104(SP)" with a real $400 frame assembles to "lea 0x128(%rsp)",
// i.e. 400+(-104)=296=0x128, not a literal -104 displacement). This
// port's earlier version used the raw offset directly -- silently
// wrong (still self-consistent for a write-then-read-the-same-slot
// round-trip like this fixture's own, so the *exit code* alone
// wouldn't have caught it; diff-amd64.sh's own byte-for-byte
// comparison against real 6a/6l is what actually catches a
// regression here).
//
// compute()'s own $32 frame, offset -8: real hardware displacement is
// 32+(-8)=24=0x18.
TEXT	_start(SB), $0
	CALL	compute+0(SB)
	MOVQ	AX, DI
	MOVQ	$60, AX
	SYSCALL

TEXT	compute+0(SB),0,$32
	MOVQ	$111,AX
	MOVQ	AX,v+-8(SP)
	MOVQ	v+-8(SP),BX
	ADDQ	$25,BX
	MOVQ	BX,AX
	RET
