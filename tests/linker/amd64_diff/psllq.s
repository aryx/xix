// Copied verbatim from goken's own tests/s/regressions/amd64_psllq.s
// (a real goken regression test, kept as-is including its own header
// comment below) -- exercises the raw GP<->XMM MOVQ bit-copy
// (Ast_asm6.ml's MovQToXmm/MovQFromXmm, real x86's "MOVQ xmm,r/m64"/
// "MOVQ r/m64,xmm", opcode 66 REX.W 0F 6E/7E -- genuinely different
// from CvtIntToF/CvtFToInt, which convert a value rather than just
// copying its raw bits) and PSLLQ's own shift-by-immediate form
// (Ast_asm6.ml's PsllQXmm, opcode 66 0F 73 /6, no REX.W).
//
// amd64_psllq: linkers/6l/optab.c's APSLLQ entry had the wrong opcode
// byte for its reg,imm8 (shift-by-immediate) encoding: 0x7e (which is
// MOVD/MOVQ, not a shift) instead of 0x73 (the correct PSLLQ opcode,
// matching APSLLQ's own memory-operand form right next to it, and
// matching the sibling APSLLL/APSLLW/APSRLQ entries, which all
// correctly use 0x72/0x71/0x73). So "PSLLQ $imm, X" assembled to a
// MOVQ instead of a shift -- confirmed by disassembly and by this
// exact test: with the bug, 1<<4 comes back as 0 (PSLLQ silently did
// nothing shift-like); with the fix, it correctly computes 16.
//
// Found via cross-arch diff analysis (scripts/diff.rc, ~/xxx/9front/
// linkers/6l/optab.c), not from a crash report -- amd64 goldens/tests
// never happened to use PSLLQ before.

TEXT _start+0(SB), 0, $0
	MOVQ	$1, AX
	MOVQ	AX, X0
	PSLLQ	$4, X0
	MOVQ	X0, DI
	MOVQ	$60, AX
	SYSCALL
