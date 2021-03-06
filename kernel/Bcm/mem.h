/*
 * Memory and machine-specific definitions.  Used in C and assembler.
 */
#define KiB     1024u           /* Kibi 0x0000000000000400 */
#define MiB     1048576u        /* Mebi 0x0000000000100000 */
#define GiB     1073741824u     /* Gibi 000000000040000000 */

/*
 * Sizes
 */
#define BY2PG       (4*KiB)         /* bytes per page */
#define PGSHIFT     12          /* log(BY2PG) */

#define MAXCPUS     4           /* max # cpus system can run */
#define CPUSIZE BY2PG
#define L1SIZE      (4 * BY2PG)

#define STACKALIGN(sp)  ((sp) & ~3)     /* bug: assure with alloc */

/*
 * Magic registers
 */
#define UP      9       /* R9 is up-> */
#define CPU     10      /* R10 is cpu-> */

/*
 * Address spaces.
 * KTZERO is used by kprof and dumpstack (if any).
 *
 * KZERO is mapped to physical 0 (start of ram).
 *
 * vectors are at 0, plan9.ini is at KZERO+256 and is limited to 16K by
 * devenv.
 */
//TODO: change to agree with Arch.kzero!
#define KSEG0       0x80000000      /* kernel segment */
/* mask to check segment; good for 1GB dram */
#define KSEGM       0xC0000000

#define KZERO       KSEG0           /* kernel address space */

#define CONFADDR    (KZERO+0x100)       /* unparsed plan9.ini */

// L1 must be 16KiB aligned so this dictates the organization below
#define CPUADDR     (KZERO+0x2000)      /* Cpu structure */
#define L2          (KZERO+0x3000)      /* L2 ptes for vectors etc */
#define VCBUFFER    (KZERO+0x3400)      /* videocore mailbox buffer */
#define FIQSTKTOP   (KZERO+0x4000)      /* FIQ stack */
#define L1          (KZERO+0x4000)      /* ttb ptes: 16KiB aligned */

#define KTZERO      (KZERO+0x8000)      /* kernel text start */

//TODO: change to agree with Arch.kzero!
#define VIRTIO      0x7E000000      /* i/o registers */
#define FRAMEBUFFER 0xC0000000      /* video framebuffer */

#define UZERO       0           /* user segment */
#define UTZERO      (UZERO+BY2PG)       /* user text start */
#define UTROUND(t)  ROUNDUP((t), BY2PG)
#define USTKTOP     0x20000000      /* user segment end +1 */
#define USTKSIZE    (8*1024*1024)       /* user stack size */
// end of new stack in sysexec
#define TSTKTOP     (USTKTOP-USTKSIZE)  /* sysexec temporary stack */
#define TSTKSIZ     256 // pages in new stack; limits exec args

/* address at which to copy and execute rebootcode */
#define REBOOTADDR  (KZERO+0x1800)

/*
 * Legacy...
 */
//old: was 64 in bcm-latest, but then get panic in _allocb
// possible alternative fix: git show 2342e706e42d1e6653c51cc1433f44816cf53fe6
#define BLOCKALIGN  32          /* only used in allocb.c */
#define KSTACK      (8*KiB)

/*
 * Sizes
 */
#define BY2WD       4
#define BY2V        8           /* only used in xalloc.c */

#define PAGETABMAPMEM   (1024*1024) // 1MB
#define PAGETABSIZE 256 // (PAGETABMAPMEM/BY2PG)
#define PAGEDIRSIZE 1984
#define SMALLPAGEDIRSIZE    16
#define PPN(x)      ((x)&~(BY2PG-1))

/*
 * Physical machine information from here on.
 *  PHYS addresses as seen from the arm cpu.
 *  BUS  addresses as seen from the videocore gpu.
 */
#define PHYSDRAM    0
#define IOSIZE      (16*MiB)
