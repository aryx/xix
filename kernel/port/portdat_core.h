
// All those structs used to be in <arch>/dat.h, but many of their fields
// were used from port/ so I've moved them here (and put the arch-specific
// fields in dat_core.h)

//*****************************************************************************
// Conf
//*****************************************************************************

// memory "bank"
struct Confmem
{
    phys_addr base;
    uintptr	limit; // used by bcm only
    ulong npage;
    kern_addr kbase; // KADDR(Confmem.base)
    kern_addr klimit; // KADDR(base+ x*BY2PG) where x <= Confmem.npage
};

struct Conf
{
    ulong ncpu;    /* processors */
    Confmem mem[4];   /* physical memory */

    ulong nproc;    /* processes */
    int copymode; /* 0 is copy on write, 1 is copy on reference */
    ulong nswap;    /* number of swap pages */
    int nswppo;   /* max # of pageouts per segment pass */
    ulong npage;    /* total physical pages of memory */

    ulong upages;   /* user page pool */ 
    // kpages = npage - upages

    ulong nimage;   /* number of page cache image headers */
    ulong ialloc;   /* max interrupt time allocation in bytes */
    ulong pipeqsize;  /* size in bytes of pipe queues */
};

extern Conf conf;

#define MAXCONF         64
// hash<string, string>
extern char *confname[];
extern char *confval[];
// Hashtbl.length(confname)
extern int nconf;

extern bool cpuserver; // defined in $CONF.c

char* getconf(char *name);

//*****************************************************************************
// Cpu
//*****************************************************************************

// =~ a jumpbuf in C, for coroutines
struct Label
{
    // or virt_addr? used also for saving context of user code?
    kern_addr sp; 
    kern_addr pc; 
};

/*
 *  performance timers, all units in perfticks
 */
struct Perf
{
    // intr-ts? interrupt time stamp?
    ulong intrts;   /* time of last interrupt */

    ulong inintr;   /* time since last clock tick in interrupt handlers */
    ulong avg_inintr; /* avg time per clock tick in interrupt handlers */

    ulong inidle;   /* time since last clock tick in idle loop */
    ulong avg_inidle; /* avg time per clock tick in idle loop */

    ulong last;   /* value of arch_perfticks() at last clock tick */
    ulong period;   /* arch_perfticks() per clock tick */
};

//coupling: do not change the order of the first 4 fields! 
// Some assembly code assumes this order.
struct Cpu
{
    int cpuno;     /* physical id of processor (KNOWN TO ASSEMBLY) */
    // must be second field at 0x04, used by arch_splhi()
    ulong splpc;      /* pc of last caller to arch_splhi */
      // ref<Proc>, or None if halting?
    Proc* proc;     /* current process on this processor */

    struct Arch_Cpu;

    ulong ticks;      /* of the clock since boot time */

    uvlong  cpuhz;
    int cpumhz; // cpuhz / 1_000_000
    Perf  perf;     /* performance counters */
    int load;
    int cs; // context switch, sched() and sleep() call
    int intr;
    int syscall;
    int pfault;
    int tlbfault;
    int tlbpurge;
    ulong spuriousintr; // not really used
    int ilockdepth;
    Label sched;      /* scheduler wakeup */ // address of schedinit()
    Proc* readied;    /* for runproc */
    ulong schedticks;   /* next forced context switch */
    bool flushmmu;   /* make current proc flush it's mmu state */
    // cyclefreq == cpuhz if havetsc, 0 otherwise
    uvlong  cyclefreq;    /* Frequency of user readable cycle counter */
    int lastintr; // debugging
  
    // must be at the end of the structure!
    int stack[1];
};


// array<ref<Cpu>>, MAXCPUS is defined in <arch>/mem.h
extern Cpu* cpus[MAXCPUS];
#define CPUS(n)  (cpus[n])

// 'up' and 'cpu' can have different implement in different arch
// so they are not defined here.

//*****************************************************************************
// Other
//*****************************************************************************

extern char* eve;
int iseve(void);
// accessed by /dev/hostdomain, defined in auth.c
extern  char  hostdomain[];

// defined in ???
extern ulong    kerndate; 

