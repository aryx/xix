#include "mem.h"
#include "arm.h"
#include "arminstr.ha"

#define MCRR(coproc, op, rd, rn, crm) \
    WORD $(0xec400000|(rn)<<16|(rd)<<12|(coproc)<<8|(op)<<4|(crm))
#define MRRC(coproc, op, rd, rn, crm) \
	WORD $(0xec500000|(rn)<<16|(rd)<<12|(coproc)<<8|(op)<<4|(crm))

TEXT tmrget(SB), 1, $-4             /* local generic timer physical counter value */
     MRRC(CpSC, 0, 1, 2, CpTIMER)
     MOVM.IA [R1-R2], (R0)
     RET
