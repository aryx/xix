
//*****************************************************************************
// Mutual exclusion
//*****************************************************************************

// arch_tas() < Lock|ILock < QLock|RWLock

// This used to be in <arch>/, but its fields are used from port/, so the 
// code must be portable!
struct Lock
{
    ulong key; // 0 when unset, 1 (or 0xDEADDEAD) when acquired, could be a bool

    bool_ushort isilock; // false when from lock(), true when from ilock()
    ulong sr; // saved priority level when using ilock() to restore in iunlock()
    // for debugging, the caller who did the lock()
    kern_addr pc; 
    // option<ref<Proc>>, None when key == 0
    Proc  *p; // the process who did the locking should be the same unlocking
    //#ifdef LOCKCYCLES
    long  lockcycles;
    //#endif
    // option<ref<Cpu>>, None when key = 0?
    Cpu  *cpu; // not that used, only in iprintcanlock apparently
};

typedef Lock ILock;

// Kernel basic lock with Queue (renamed to avoid ambiguity with libc.h QLock)
struct KQLock
{
    bool  locked;   /* flag */
  
    // list<ref<Proc>> (next = Proc.qnext)
    Proc  *head;    /* next process waiting for object */
    // option<ref<Proc>> (direct access to tail, queue)
    Proc  *tail;    /* last process waiting for object */

    kern_addr qpc;    /* pc of the holder */ // for debugging?
  
    Lock  use;    /* to access Qlock structure */
};

struct RWlock
{
    int readers;  /* number of readers */
    bool writer;   /* have a writer? */
  
    // list<ref<Proc>> (next = Proc.qnext)
    Proc  *head;    /* list of waiting processes */
    // option<ref<Proc>> (direct access to tail, queue)
    Proc  *tail;
    // option<ref<Proc>> 
    Proc  *wproc;   /* writing proc */
  
    uintptr wpc;    /* pc of writer */
  
    Lock  use;
};

//*****************************************************************************
// Atomicity
//*****************************************************************************

// For reference counting shared things (e.g. a Page)
struct Ref
{
    long ref;
    Lock;
};

typedef struct Ref Counter;

//*****************************************************************************
// Synchronization
//*****************************************************************************

// defined in this directory but no functions are operating on it in this dir
struct Rendez
{
    // option<ref<Proc>>
    Proc  *p; // sleeping process
    Lock;
};

// user level semaphores, used to implement user-level lock, 
// see libc/port/lock.c
struct Sema
{
    long  *addr; // value stored in user space!
    bool waiting;
  
    //list<Sema> of Segment.sema
    Sema  *next;
    Sema  *prev;

    Rendez;
};

// see also Waitq in portdat_processes.h?
