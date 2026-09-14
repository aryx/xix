// Real amd64 TEXT's own frame-size operand isn't just documentation --
// goken's real 6a automatically synthesizes a "SUB $autosize,SP" right
// after TEXT and an "ADD $autosize,SP" right before every RET in that
// function. This port's earlier version emitted nothing at all for
// TEXT (a real bug, invisible at assemble/link time -- every
// individual instruction's own encoding was already correct, only the
// enclosing function's own SP adjustment was missing entirely -- see
// Rewrite6.ml's own add_prologue_epilogue comment).
TEXT	_start(SB), $0
	CALL	frame+0(SB)
	MOVQ	AX, DI
	MOVQ	$60, AX
	SYSCALL

TEXT	frame+0(SB),0,$24
	MOVQ	$7,AX
	MOVQ	AX,(SP)
	MOVQ	(SP),AX
	ADDQ	$3,AX
	RET
