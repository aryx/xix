
// in lib.h: Waitmsg, ERRMAX

//*****************************************************************************
// Proc components
//*****************************************************************************
// All the ref<Proc> here are references to Proc in the array<Proc> of
// Procalloc.arena (pool allocator)

//--------------------------------------------------------------------
// State
//--------------------------------------------------------------------

// TODO: state transition diagram
/* Process states, Proc.state */
enum Procstate
{
    Dead = 0,
    Running,
    Queueing, // see qlock()
    QueueingR, // see rlock()
    QueueingW, // see wlock()
    Scheding,
    Moribund,
    Ready,
    Wakeme,
    Rendezvous,
    Broken,
    Stopped,
    Waitrelease, // for real-time scheduling
};

// hash<enum<procstate>, string>, to debug
extern  char* statename[];

//--------------------------------------------------------------------
// Memory
//--------------------------------------------------------------------
/*
 *  process memory segments - NSEG always last !
 */
enum Procseg
{
    SSEG, TSEG, DSEG, BSEG, // Stack, Text, Data, Bss

    ESEG, // E = Extra (used for temporary stack segment),
    _SEG0, _SEG1, _SEG2, _SEG3, _SEG4, // free slots for for segattach

    NSEG // to count, see Proc.seg array
};

//--------------------------------------------------------------------
// Files
//--------------------------------------------------------------------

enum
{
       DELTAFD = 20    /* incremental increase in Fgrp.fd's */
};

struct Fgrp
{
    // array<option<ref_counted<Chan>>>
    Chan  **fd;
    // nelem(fd)
    int nfd;      /* number allocated */
    int maxfd;      /* highest fd in use */ // <= nfd
  
    int exceed;     /* debugging */
    // extra
    Ref;
};


enum
{
    MNTLOG  = 5,
    MNTHASH = 1<<MNTLOG,  /* Hash to walk mount table */
};
#define MOUNTH(p,qid) ((p)->mnthash[(qid).path&((1<<MNTLOG)-1)])

// Mount table, aka Namespace, aka process group
struct Pgrp
{
    // hash<qid.path, list<ref<Mhead> (next = Mhead.next)>
    Mhead *mnthash[MNTHASH];

    ulong pgrpid;
    bool noattach;
  
    // extra
    Ref;        /* also used as a lock when mounting */

    // ORDER OF LOCK: first debug and then ns
    QLock debug;      /* single access via devproc.c */
    RWlock  ns;     /* Namespace n read/one write lock */
};

//--------------------------------------------------------------------
// System call
//--------------------------------------------------------------------

#define MAXSYSARG  5 /* for mount(fd, afd, mpt, flag, arg) */

// syscall arguments copied from user stack
struct Sargs
{
    ulong args[MAXSYSARG];
};

//--------------------------------------------------------------------
// Notes
//--------------------------------------------------------------------

enum {
    NNOTE = 5,
};

enum NoteKind
{
    NUser,        /* note provided externally */
    NExit,        /* deliver note quietly */
    NDebug,       /* print debug message */
};

// a kind of Unix signal
struct Note
{
    char  msg[ERRMAX];
    // enum<NoteKind>
    int flag;     /* whether system posted it */
};
extern Counter  noteidalloc;

//--------------------------------------------------------------------
// Process children waiting
//--------------------------------------------------------------------

// essentially a stack<ref_own<Waitmsg>>
struct Waitq
{
    Waitmsg w;
  
    // extra
    // list<ref_own<Waitq>> Proc.waitq
    Waitq *next;
};

//--------------------------------------------------------------------
// Synchronization (Rendez vous)
//--------------------------------------------------------------------

enum
{
    RENDLOG = 5,
    RENDHASH =  1<<RENDLOG, /* Hash to lookup rendezvous tags */
};
#define REND(p,s) ((p)->rendhash[(s)&((1<<RENDLOG)-1)])

struct Rgrp
{
    // hash<??, list<ref<Proc>>>
    Proc  *rendhash[RENDHASH];  /* Rendezvous tag hash */
  
    // extra
    Ref;        /* the Ref's lock is also the Rgrp's lock */
};

//--------------------------------------------------------------------
// Alarms, timers
//--------------------------------------------------------------------
// now in portdat_time.h

//--------------------------------------------------------------------
// Scheduling
//--------------------------------------------------------------------

enum {
    Npriq   = 20,   /* number of scheduler priority levels */
    Nrq   = Npriq+2,  /* number of priority levels including real time */
};

enum Priority 
{
    PriNormal = 10,   /* base priority for normal processes */
    PriKproc  = 13,   /* base priority for kernel processes */
    PriRoot   = 13,   /* base priority for root processes */

    PriRelease  = Npriq,  /* released edf processes */
    PriEdf    = Npriq+1,  /* active edf processes */
    PriExtra  = Npriq-1,  /* edf processes at high best-effort pri */
};

enum EdfFlags 
{
    /* Edf.flags field */
    Admitted    = 0x01,
    Sporadic    = 0x02,
    Yieldonblock    = 0x04,
    Sendnotes   = 0x08,
    Deadline    = 0x10,
    Yield     = 0x20,
    Extratime   = 0x40,
};

struct Edf {
    /* All times in µs */
    /* time intervals */
    long    D;    /* Deadline */
    long    Delta;    /* Inherited deadline */
    long    T;    /* period */
    long    C;    /* Cost */
    long    S;    /* Slice: time remaining in this period */
    /* times (only low-order bits of absolute time) */
    long    r;    /* (this) release time */
    long    d;    /* (this) deadline */
    long    t;    /* Start of next period, t += T at release */
    long    s;    /* Time at which this proc was last scheduled */
  
    /* for schedulability testing */
    long    testDelta;
    int   testtype; /* Release or Deadline */
    long    testtime;
    Proc    *testnext;
  
    /* other */
    // set<enum<edfflags>>
    ushort    flags;
  
    Timer;
  
    /* Stats */
    long    edfused;
    long    extraused;
    long    aged;
    ulong   periods;
    ulong   missed;
};

//--------------------------------------------------------------------
// Error management
//--------------------------------------------------------------------
enum {
    NERR = 64,
};

//--------------------------------------------------------------------
// Stats, profiling
//--------------------------------------------------------------------
enum Proctime 
{
    TUser = 0,    /* Proc.time */
    TSys,
    TReal,

    // to accumulate also the time of the children of the process
    TCUser,
    TCSys,
};

//--------------------------------------------------------------------
// Debugger
//--------------------------------------------------------------------

enum Procctl
{
    Proc_nothing = 0,
    Proc_stopme,
    Proc_exitme,
    Proc_tracesyscall,
    Proc_traceme,
    Proc_exitbig,
};

//--------------------------------------------------------------------
// Misc
//--------------------------------------------------------------------

/*
 * FPsave.status
 */
enum FPSaveStatus
{
    /* this is a state */
    FPinit=   0,

    FPactive= 1,
    FPinactive= 2,

    /* the following is a bit that can be or'd into the state */
    FPillegal=  0x100,
};

//*****************************************************************************
// Proc, the big one
//*****************************************************************************

struct Proc
{
//--------------------------------------------------------------------
//coupling: have to be first! Some assembly code assumes this order.
//--------------------------------------------------------------------
    Label sched;    /* known to l.s */
    char  *kstack;  /* known to l.s */
//--------------------------------------------------------------------
// State
//--------------------------------------------------------------------
    // enum<Procstate> 
    int state; // Dead, Queuing, etc, (used by /proc/#/status if psstate==nil)
    ulong pid;
    // some debugging information, e.g. "New", "PageOut", or name of syscall
    char  *psstate; /* used by /proc/#/status */
    bool insyscall; // true when process inside a syscall

    // e.g. "*init*", or name of executable
    char  *text;
    // e.g.. "eve" (no uid/gid in plan9, because of its distributed nature?)
    char  *user;
    ulong parentpid;
    char  *args;
    int nargs;    /* number of bytes of args */
//--------------------------------------------------------------------
// Memory
//--------------------------------------------------------------------
    // hash<enum<procseg>, option<ref_own<Segment>>>, elt smalloc'ed? ref_counted?
    Segment *seg[NSEG];
    QLock seglock;  /* locked whenever seg[] changes */
    bool newtlb;   /* Pager has changed my pte's, I must flush */
    bool noswap;   /* process is not swappable */

    struct Arch_Proc;
//--------------------------------------------------------------------
// Scheduling
//--------------------------------------------------------------------
    // enum<priority>
    ulong priority; /* priority level */

    ulong basepri;  /* base priority level */
    bool fixedpri; /* priority level doesn't change */
    // option<ref<Cpu>>, null when not associated to a processor
    Cpu  *cpu;    /* processor running this proc */
    Cpu *lastcpu;    /* processor this process last ran on */
    ulong lastupdate; // dimension?? ticks * Scaling;
    ulong cpuavg;    /* cpu average */
    ulong delaysched;
    bool preempted;  /* true if this process hasn't finished the interrupt
           *  that last preempted it
           */
    Cpu  *wired;
    // option<ref_own?<edf>>
    Edf *edf; /* if non-null, real-time proc, edf contains scheduling params */
    ulong readytime;  /* time process came ready */
//--------------------------------------------------------------------
// Files
//--------------------------------------------------------------------
    // ref<Chan>
    Chan  *slash; // The root! used by namec()
    // ref_counted<Chan>
    Chan  *dot; // The current directory
    // ref_counted<fgrp>
    Fgrp  *fgrp;    /* File descriptor group */
    Fgrp  *closingfgrp; /* used during teardown */
    // ref_counted<pgrp>
    Pgrp  *pgrp;    /* Process group for namespace */
//--------------------------------------------------------------------
// Notes
//--------------------------------------------------------------------
    Note  note[NNOTE];
    short nnote;

    int (*notify)(void*, char*);
    bool_ushort notified; /* sysnoted is due */

    ulong noteid;   /* Equivalent of note group */

    bool notepending;  /* note issued but not acted on */

    Note  lastnote;

    void  *ureg;    /* User registers for notes */
//--------------------------------------------------------------------
// Process hierarchy
//--------------------------------------------------------------------
    // option<ref<Proc>> nil for the boot process
    Proc  *parent;
    int nchild;   /* Number of living children */
    // list<ref_own<Waitq>>> =~ list<ref_own<Waitmsg>>
    Waitq *waitq;   /* Exited processes wait children */
    int nwait;    /* Number of uncollected wait records */ // len(waitq)
    Lock  exl;    /* Lock count and waitq */
    Rendez  waitr;    /* Place to hang out in wait */
    QLock qwaitr;
//--------------------------------------------------------------------
// Synchronization
//--------------------------------------------------------------------
    // As long as the current process hold spinlocks (to kernel data structures),
    // we will not schedule another process in unlock(); only the last unlock
    // will eventually cause a rescheduling.
    Ref nlocks;   /* number of locks held by proc */
    // option<ref<Rendez>>, can point to waitr, freememr, sleepr, etc
    Rendez  *r;   /* rendezvous point slept on */
    Lock  rlock;    /* sync sleep/wakeup with postnote */
    Rendez  sleepr;    /* place for syssleep/debug/tsleep */
    Rgrp  *rgrp;    /* Rendez group */

    uintptr rendtag;  /* Tag for rendezvous */
    uintptr rendval;  /* Value for rendezvous */
    //??
    Proc  *rendhash;  /* Hash list for tag values */
//--------------------------------------------------------------------
// Error management
//--------------------------------------------------------------------
    // array<Label>, error labels, poor's man exceptions in C
    Label errlab[NERR];
    // length(errlab) used.
    int nerrlab;

    // ref<string> point to errbuf0 or to syserrstr (which points to errbuf1)
    char  *errstr;  /* reason we're unwinding the error stack, errbuf1 or 0 */
    char  errbuf0[ERRMAX];
    char  errbuf1[ERRMAX];
    char  *syserrstr; /* last error from a system call, errbuf0 or 1 */
//--------------------------------------------------------------------
// Stats, profiling
//--------------------------------------------------------------------
    // hash<enum<proctime>, ulong>
    ulong time[5];  /* User, Sys, Real; child U, S */
    uvlong  kentry;   /* Kernel entry time stamp (for profiling) */
    /*
     * pcycles: cycles spent in this process (updated on procsave/restore)
     * when this is the current proc and we're in the kernel
     * (procrestores outnumber procsaves by one)
     * the number of cycles spent in the proc is pcycles + cycles()
     * when this is not the current process or we're in user mode
     * (procrestores and procsaves balance), it is pcycles.
     */
    vlong pcycles;
//--------------------------------------------------------------------
// Debugging (the kernel itself)
//--------------------------------------------------------------------
    Lock* lastlock;
    Lock  *lastilock;
    ulong qpc;    /* pc calling last blocking qlock */
//--------------------------------------------------------------------
// For debugger, strace
//--------------------------------------------------------------------
    void  *dbgreg;  /* User registers for devproc */
    // enum<procctl>
    int procctl;  /* Control for /proc debugging */
    Proc  *pdbg;    /* the debugging process */
    QLock debug;    /* to access debugging elements */ // used for many things
    bool hang;   /* hang at next exec for debug */
    char  *syscalltrace;  /* syscall trace */
    bool trace;    /* process being traced? */
    bool setargs;
//--------------------------------------------------------------------
// Other
//--------------------------------------------------------------------
    bool kp;   /* true if a kernel process */
    void  (*kpfun)(void*);
    void  *kparg;
    Sargs sargs;    /* address of this is known by db */
    char  genbuf[128];  /* buffer used e.g. for last name element from namec */
    Timer;      /* For tsleep and real-time */
    Rendez  *trend;
    int (*tfn)(void*);
    ulong alarm;    /* Time of call */
    // enum<FPSaveStatus>
    int fpstate;
    Arch_FPsave  fpsave;   /* address of this is known by db */
    // ref_counted<Egrp>
    Egrp  *egrp;    /* Environment group */
    ulong procmode; /* proc device default file mode */
    bool privatemem; /* proc does not let anyone read mem */
//--------------------------------------------------------------------
// Extra
//--------------------------------------------------------------------
    // list<ref<Proc>> KQlock.head or RWLock.head (or Procalloc.free)
    Proc  *qnext;   /* next process on queue for a QLock */
    // hash<Proc.pid, ref<Proc>> Procalloc.ht
    Proc  *pidhash; /* next proc in pid hash */ 
    // list<ref<Proc>> of Schedq.head
    Proc  *rnext;   /* next process in run queue */
    // Alarms.head chain?
    Proc  *palarm;  /* Next alarm time */
};

// poor's man exceptions in C
//  - waserror() =~ try  
//     * if (!waserror()) { } else { } <=> try { } catch { }
//     * if (waserror()) { }  <=> finally { }
//  - poperror() = nothing
//  - error() =~ raise
//  - nexterror() =~ re raise from exn handler
// note, arch_setlabel() return false, so the branch is never taken first
// but nexterror() is using arch_gotolabel() which returns true, see l_switch.s
#define waserror()  (up->nerrlab++, arch_setlabel(&up->errlab[up->nerrlab-1]))
#define poperror()    up->nerrlab--


//*****************************************************************************
// Internal to process/
//*****************************************************************************

// Proc allocator (singleton), was actually in proc.c, but important so here
struct Procalloc
{
    // array<Proc>, xalloc'ed in procinit() (conf.nproc)
    Proc* arena;
  
    // list<ref<Proc>> (next = Proc.qnext, hmmm abuse qnext)
    Proc* free;

    // hash<Proc.pid, ref<Proc>> (next = Proc.pidhash)>
    Proc* ht[128];
  
    // extra
    Lock;
};
//IMPORTANT: static struct Procalloc procalloc; (in proc.c)

// essentially a queue<ref<Proc>>
struct Schedq
{
    // list<ref<Proc>> (next = Proc.rnext)
    Proc* head;
    // ref<Proc>, the tail
    Proc* tail;
    // size of list
    int n; 
  
    // extra
    Lock;
};
// hash<enum<priority>, Schedq>, Nrq is the number of priority level (20+2)
//IMPORTANT: Schedq  runq[Nrq];  (in proc.c)

// was in alarm.c, but important so here
struct Alarms
{
    // list<ref<Proc> (next = Proc.palarm)
    Proc  *head;
    // extra
    QLock;
};
//IMPORTANT: static Alarms alarms; (in alarm.c)
//IMPORTANT: static Rendez alarmr; (in alarm.c)

struct Active
{
    // array<bool> (coupling: sizeof(int) must be >= MAXCPUS)
    int cpus;      /* bitmap of active CPUs */
    bool exiting;    /* shutdown */
    bool ispanic;    /* shutdown in response to a panic */
    bool rebooting;    /* just idle cpus > 0 */
    bool main_reached_sched;/* lets the added processors continue to schedinit*/
    // extra
    Lock;
};
extern struct Active active;

