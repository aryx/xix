
typedef u32int      PTE;

//coupling: do not change the first field! Some assembly code assumes this order
struct Arch_Cpu {
    PTE*    mmul1;      /* l1 for this processor */
    // index in mmul1
    int mmul1lo;
    int mmul1hi;
    int mmupid;
    // option<VFPKind>, None = 0 /* vfp2 or vfp3 fpu */
    int havefp;
    bool havefpvalid;
    // 16 for VFPv2, 32 for VFPv3
    int fpnregs;
    bool fpon;
    ulong   fpscr;          /* sw copy */
    bool fpconfiged;
    int fppid;          /* pid of last fault */
    uintptr fppc;       /* addr of last fault */
    int fpcnt;          /* how many consecutive at that addr */
    /* save areas for exceptions, hold R0-R4 */
    u32int  sirq[5];
    u32int  sund[5];
    u32int  sabt[5];
    uvlong  fastclock;      /* last sampled value */
};
