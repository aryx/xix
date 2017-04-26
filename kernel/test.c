#include <u.h>
#include "port/lib.h"
#include "port/error.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"

#include "arm.h"

#define ARMLOCAL    (VIRTIO+IOSIZE)

enum {
    Localctl    = 0x00,
    Prescaler   = 0x08,

    /* generic timer (cortex-a7) */
    Enable  = 1<<0,
    Imask   = 1<<1,
    Istatus = 1<<2,
};

extern uvlong tmrget(void);
extern uvlong tmrcmpget(void);
extern void   tmrcmpset(void);

extern void   xxxset(void);
extern int   xxxget(void);

void test(void) {
    uvlong v;
    int x;
	//volatile u32int *addr;
    //addr = (volatile u32int *) (ARMLOCAL + 0x0040); 
    //*addr = 0x1; // enable CNTPSIRQ

    x = cprdsc(0, CpTIMER, 0, 0);
    print("freq = %d\n", x);

    v = tmrget();
    print("val1: %lld\n", v);

    cpwrsc(0, CpTIMER, CpTIMERphys, CpTIMERphysval, 62500000);
    cpwrsc(0, CpTIMER, CpTIMERphys, CpTIMERphysctl, Enable);
    
    //xxxset();
    //print("val = %d", xxxget());

    //assert(arch_islo());
    arch_spllo();
}

void test3(void) {
    uvlong v;
    unsigned int x;

    v = tmrget();
    print("val1: %lld\n", v);
    while (v < 200 * 1000 * 1000) {
      v = tmrget();
    }
    v = tmrget();
    print("val2: %lld\n", v);

    tmrcmpset();

    v = tmrcmpget();
    print("cmp: %lld\n", v);


    //x = (unsigned int) cprdsc(0, CpTIMER, CpTIMERphys, CpTIMERphysval);
    //print("val1: %d\n", x);
    //x = cprdsc(0, CpTIMER, CpTIMERphys, CpTIMERphysval);
    //print("val2: %d\n", x);
    //
    cpwrsc(0, CpTIMER, CpTIMERphys, CpTIMERphysval, 5000);
    cpwrsc(0, CpTIMER, CpTIMERphys, CpTIMERphysctl, Enable);


}

void test2(void) {
    ulong x, y;
    uvlong v;


    *(ulong*)(ARMLOCAL + Localctl) = 0;             /* magic */
    *(ulong*)(ARMLOCAL + Prescaler) = 0x06aaaaab;   /* magic for 1 Mhz */

    arch_coherence();

    x = *(ulong*)(ARMLOCAL + 0x1C);
    y = *(ulong*)(ARMLOCAL + 0x20);
    print("%d %d\n", x, y); // does not work, get 0 0

    // cpwrsc(0, CpTIMER, 0, 0, 1000000); // does not change anything

    x = cprdsc(0, CpTIMER, 0, 0);
    print("freq = %d\n", x);

    v = tmrget();
    print("val1: %lld\n", v);
    while (v < 200 * 1000 * 1000) {
      v = tmrget();
    }
    v = tmrget();
    print("val2: %lld\n", v);

    x = cprdsc(0, CpTIMER, CpTIMERphys, CpTIMERphysval);
    print("val1: %ld\n", x);
    x = cprdsc(0, CpTIMER, CpTIMERphys, CpTIMERphysval);
    print("val2: %ld\n", x);

    // Some tests
    void* x1;
    x1 = malloc(10000);
    void* x2;
    x2 = malloc(100);
    print("Fuck yeah!%p, %p\n", x1, x2); // yeah!

    //*(byte*)13 = 1;
}
