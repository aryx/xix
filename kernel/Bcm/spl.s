#include "mem.h"
#include "arm.h"
#include "arminstr.ha"

// bool   arch_splhi(void);
TEXT arch_splhi(SB), 1, $-4
    MOVW    $(CPUADDR+4), R2        /* save caller pc in Cpu */
    MOVW    R14, 0(R2)
    MOVW    CPSR, R0            /* turn off irqs (but not fiqs) */
    ORR $(PsrDirq), R0, R1
    MOVW    R1, CPSR
    RET
        
// bool   arch_spllo(void);
TEXT arch_spllo(SB), 1, $-4
    MOVW    CPSR, R0            /* turn on irqs and fiqs */
    BIC $(PsrDirq|PsrDfiq), R0, R1
    MOVW    R1, CPSR
    RET
        
// void    arch_splx(bool);
TEXT arch_splx(SB), 1, $-4
    MOVW    $(CPUADDR+4), R2        /* save caller pc in Cpu */
    MOVW    R14, 0(R2)
    MOVW    R0, R1              /* reset interrupt level */
    MOVW    CPSR, R0 //dead
    MOVW    R1, CPSR
    RET

// bool arch_islo(void);
TEXT arch_islo(SB), 1, $-4
    MOVW    CPSR, R0
    AND $(PsrDirq), R0
    EOR $(PsrDirq), R0
    RET


TEXT arm_arch_coherence(SB), $-4
    BARRIERS
    RET
