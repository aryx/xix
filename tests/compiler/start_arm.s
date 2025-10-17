TEXT _start(SB), $0
#ifdef arm_
	// needed for 5l_; without this only "Hello C world"
	// is printed and not "It works!"
	MOVW $setR12(SB), R12
#endif
	BL main(SB)
