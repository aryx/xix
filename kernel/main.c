#include <u.h>
#include "port/lib.h"
#include "port/error.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"

// for mainmem and imagmem
#include <pool.h>

void arm_arch_coherence(void);
int screen_print(char *fmt, ...);
void screen_panic(char *fmt, ...);

//*****************************************************************************
// Cpu init
//*****************************************************************************
void
arch__cpuinit(void)
{
    Cpu *m0;

    cpu->ticks = 1;
    cpu->perf.period = 1;

    m0 = CPUS(0);
    /*s: [[arch__cpuinit()]] it not cpu0 */
    if (cpu->cpuno != 0) {
        /* synchronise with cpu 0 */
        cpu->ticks = m0->ticks;
        cpu->fastclock = m0->fastclock;
    }
    /*e: [[arch__cpuinit()]] it not cpu0 */
}

void
arch__cpu0init(void)
{
    conf.ncpu = 0; // set in machon() instead (machon() is called after cpuinit)

    cpu->cpuno = 0;
    cpus[cpu->cpuno] = cpu;

    arch__cpuinit();
    //active.exiting = 0;

    up = nil;
}

//*****************************************************************************
// Conf init
//*****************************************************************************

// used also in mmu.c
ulong   memsize = 128*1024*1024;

void
arch__confinit(void)
{
    int i;
    char *p;
    phys_addr pa;
    ulong kpages;
    ulong kmem;

    if((p = getconf("*maxmem")) != nil){
        memsize = strtoul(p, 0, 0);
        if (memsize < 16*MB)        /* sanity */
            memsize = 16*MB;
    }
    // simpler than for x86 :)
    getramsize(&conf.mem[0]);

    if(conf.mem[0].limit == 0){
        conf.mem[0].base = 0;
        conf.mem[0].limit = memsize;
    }else if(p != nil)
        conf.mem[0].limit = conf.mem[0].base + memsize;

    conf.npage = 0;
    pa = PADDR(PGROUND(PTR2UINT(end)));

    /*
     *  we assume that the kernel is at the beginning of one of the
     *  contiguous chunks of memory and fits therein.
     */
    for(i=0; i<nelem(conf.mem); i++){
        /* take kernel out of allocatable space */
        if(pa > conf.mem[i].base && pa < conf.mem[i].limit)
            conf.mem[i].base = pa;

        conf.mem[i].npage = (conf.mem[i].limit - conf.mem[i].base)/BY2PG;
        conf.npage += conf.mem[i].npage;
    }

    conf.upages = (conf.npage*80)/100;
    kpages = conf.npage - conf.upages;

    /* set up other configuration parameters */
    conf.ialloc = (kpages/2)*BY2PG; // max bytes for iallocb

    conf.nproc = 100 + ((conf.npage*BY2PG)/MB)*5;
    if(conf.nproc > 2000)
        conf.nproc = 2000;

    conf.nswap = conf.npage*3;
    conf.nswppo = 4096;
    conf.nimage = 200;

    conf.copymode = 1;      /* copy on reference, not copy on write */

    /*
     * Guess how much is taken by the large permanent
     * datastructures. Mntcache and Mntrpc are not accounted for
     * (probably ~300KB).
     */
    kmem = kpages * BY2PG;
    kmem -= 
          conf.upages*sizeof(Page)
        + conf.nproc*sizeof(Proc)
        + conf.nimage*sizeof(KImage)
        + conf.nswap
        + conf.nswppo*sizeof(Page*); // pad's second bugfix :)

    // memory pool
    mainmem->maxsize = kmem;

    /*
     * give terminals lots of image memory, too; the dynamic
     * allocation will balance the load properly, hopefully.
     * be careful with 32-bit overflow.
     */
    imagmem->maxsize = kmem;
}

//*****************************************************************************
// Main entry point!
//*****************************************************************************

extern void caml_startup(char **argv);

void
main(void)
{
    arch_coherence = arm_arch_coherence;
    print = screen_print;
    panic = screen_panic;

    memset(edata, 0, end - edata);  /* clear bss */

    // Let's go!

    cpu = (Cpu*)CPUADDR;
    arch__cpu0init(); // cpu0 initialization (calls arch__cpuinit())
    mmuinit1((void*)L1); // finish mmu initialization started in mmuinit0

    arch__confinit();     /* figures out amount of memory */
    xinit();

    arch__screeninit(); // screenputs = swconsole_screenputs
    
    quotefmtinstall(); // libc printf initialization
    print("\nPlan 9 from Bell Labs\n"); // yeah!

    void* x1;
    x1 = malloc(10000);
    void* x2;
    x2 = malloc(100);
    print("Fuck yeah!%p, %p\n", x1, x2); // yeah!

    caml_startup(nil);

    print("Done!"); // yeah!
    assert(0);          /* shouldn't have returned */
}

