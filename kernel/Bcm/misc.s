#include "mem.h"
#include "arm.h"
#include "arminstr.ha"

#define MCRR(coproc, op, rd, rn, crm) \
    WORD $(0xec400000|(rn)<<16|(rd)<<12|(coproc)<<8|(op)<<4|(crm))
#define MRRC(coproc, op, rd, rn, crm) \
	WORD $(0xec500000|(rn)<<16|(rd)<<12|(coproc)<<8|(op)<<4|(crm))

TEXT tmrcmpget(SB), 1, $-4
     MRRC(CpSC, 2, 1, 2, CpTIMER)
     MOVM.IA [R1-R2], (R0)
     RET

TEXT tmrcmpset(SB), 1, $-4
     MOVW $0, R2
     MOVW $(400 * 1000 * 1000), R1
     MCRR(CpSC, 2, 1, 2, CpTIMER)
     RET

        
TEXT xxxset(SB), 1, $-4
    MOVW $(0x1), R0            
	MCR CpSC, 0, R0, C(14), C(2), 1

TEXT xxxget(SB), 1, $-4
	MRC CpSC, 0, R0, C(14), C(2), 1
                                