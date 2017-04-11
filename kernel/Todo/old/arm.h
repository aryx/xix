// Last 4 bits of PSR
#define PsrMusr     0x00000010  /* user mode */
#define PsrMsvc     0x00000013  /* `protected mode for OS' */

#define PsrMirq     0x00000012
#define PsrMabt     0x00000017
#define PsrMund     0x0000001B
#define PsrMfiq     0x00000011

#define PsrDirq     0x00000080      /* disable IRQ interrupts */
#define PsrDfiq     0x00000040      /* disable FIQ interrupts */


#define CpSC        15          /* System Control */

#define CpCONTROL   1           /* miscellaneous control */
#define CpTTB       2           /* Translation Table Base(s) */
#define CpDAC       3           /* Domain Access Control */
#define CpFSR       5           /* Fault Status */
#define CpFAR       6           /* Fault Address */
#define CpCACHE     7           /* cache/write buffer control */
#define CpTLB       8           /* TLB control */
#define CpSPM       15          /* system performance monitor (arm1176) */

#define CpMainctl   0

#define CpCmmu      0x00000001  /* M: MMU enable */
#define CpChv       0x00002000  /* V: high vectors */
#define CpCdcache   0x00000004  /* C: data cache on */
#define CpCicache   0x00001000  /* I: instruction cache on */
