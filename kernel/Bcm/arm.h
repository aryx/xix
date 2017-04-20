/*
 * ARM-specific definitions for armv6 (arm11), armv7 (cortex-a7 and -a8)
 * these are used in C and assembler
 */

/*
 * Program Status Registers
 */
// Last 4 bits of PSR
#define PsrMusr     0x00000010  /* user mode */
#define PsrMsvc     0x00000013  /* `protected mode for OS' */

#define PsrMirq     0x00000012
#define PsrMabt     0x00000017
#define PsrMund     0x0000001B
#define PsrMfiq     0x00000011

#define PsrMask     0x0000001F

#define PsrDirq     0x00000080      /* disable IRQ interrupts */
#define PsrDfiq     0x00000040      /* disable FIQ interrupts */

/*
 * Coprocessors
 */
#define CpSC        15          /* System Control */
#define CpFP        10          /* float FP, VFP cfg. */
#define CpDFP       11          /* double FP */

/*
 * Primary (CRn) CpSC registers.
 */
#define CpCONTROL   1           /* miscellaneous control */
#define CpTTB       2           /* Translation Table Base(s) */
#define CpDAC       3           /* Domain Access Control */
#define CpFSR       5           /* Fault Status */
#define CpFAR       6           /* Fault Address */
#define CpTLB       8           /* TLB control */
#define CpSPM       15          /* system performance monitor (arm1176) */
#define CpCACHE     7           /* cache/write buffer control */
#define CpID        0           /* ID and cache type */
#define CpTIMER     14          /* Generic timer (cortex-a7) */
#define CpCLD       9        // Performance monitor

// CpSC secondary registers

/*
 * CpID Secondary (CRm) registers.
 */
#define CpIDidct    0
#define CpIDfeat    1
/*
 * CpID op1==0 opcode2 fields.
 * the cortex has more op1 codes for cache size, etc.
 */
#define CpIDid      0           /* main ID */
#define CpIDmpid    5           /* multiprocessor id (cortex) */

/*
 * CpCONTROL op2 codes, op1==0, Crm==0.
 */
#define CpMainctl   0
#define CpCPaccess  2
#define CpAuxctl    1
/*
 * CpCONTROL: op1==0, CRm==0, op2==CpMainctl.
 * main control register.
 * cortex/armv7 has more ops and CRm values.
 */
#define CpCmmu      0x00000001  /* M: MMU enable */
#define CpChv       0x00002000  /* V: high vectors */
#define CpCdcache   0x00000004  /* C: data cache on */
#define CpCicache   0x00001000  /* I: instruction cache on */
#define CpCsw       (1<<10)     /* SW: SWP(B) enable (deprecated in v7) */
#define CpCpredict  0x00000800  /* Z: branch prediction (armv7) */
#define CpCve       (1<<24)     /* VE: intr vectors enable */
#define CpCtre      (1<<28)     /* TRE: TEX remap enable */
#define CpCsbo (3<<22|1<<18|1<<16|017<<3)       /* must be 1 (armv7) */
#define CpCsbz (CpCtre|1<<26|CpCve|1<<15|7<<7)  /* must be 0 (armv7) */

/*
 * CpTTB op1==0, Crm==0 opcode2 values.
 */
#define CpTTB1      1           /* cortex */

/*
 * CpFSR opcode2 values.
 */
#define CpFSRdata   0           /* armv6, armv7 */
#define CpFSRinst   1           /* armv6, armv7 */


/*
 * CpTLB Secondary (CRm) registers and opcode2 fields.
 */
#define CpTLBinvu   7           /* unified */
#define CpTLBinv    0           /* invalidate all */
#define CpTLBinvse  1           /* invalidate single entry */

/*
 * CpCLD Secondary (CRm) registers and opcode2 fields for op1==0. (cortex)
 */
#define CpCLDena    12          /* enables */
#define CpCLDcyc    13          /* cycle counter */
#define CpCLDenapmnc    0
#define CpCLDenacyc 1

/*
 * CpTIMER op1==0 Crm and opcode2 registers (cortex-a7)
 */
#define CpTIMERphys     2

#define CpTIMERphysval  0
#define CpTIMERphysctl  1

/*
 * CpSPM Secondary (CRm) registers and opcode2 fields (armv6)
 */
#define CpSPMperf   12          /* various counters */
#define CpSPMctl    0           /* performance monitor control */
#define CpSPMcyc    1           /* cycle counter register */


// Misc

/*
 * CpTTB cache control bits
 */
#define CpTTBs  (1<<1)  /* page table in shareable memory */

/*
 * MMU page table entries.
 */
/* Mbz (0x10) bit is implementation-defined and must be 0 on the cortex. */
#define Mbz     (0<<4)

#define Fault       0x00000000      /* L[12] pte: unmapped */

#define Coarse      (Mbz|1)         /* L1 */
#define Section     (Mbz|2)         /* L1 1MB */
#define Fine        (Mbz|3)         /* L1 */

#define Large       0x00000001      /* L2 64KB */
#define Small       0x00000002      /* L2 4KB */

// simplified caching for now: No cache!
//#define Buffered    0x00000004      /* L[12]: write-back not -thru */
//#define Cached      0x00000008      /* L[12] */

#define Dom0        0

#define Noaccess    0       /* AP, DAC */
#define Krw     1           /* AP */
/* armv7 deprecates AP[2] == 1 & AP[1:0] == 2 (Uro), prefers 3 (new in v7) */
#define Uro     2           /* AP */
#define Urw     3           /* AP */

#define Client      1           /* DAC */
#define Manager     3           /* DAC */

#define F(v, o, w)  (((v) & ((1<<(w))-1))<<(o))

#define AP(n, v)    F((v), ((n)*2)+4, 2)
#define L1AP(ap)    (AP(3, (ap)))
/* L2AP differs between armv6 and armv7 -- see l2ap in arch*.c */
#define DAC(n, v)   F((v), (n)*2, 2)

#define HVECTORS    0xffff0000

