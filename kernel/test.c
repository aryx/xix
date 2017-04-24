#include <u.h>
#include "port/lib.h"
#include "port/error.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"

// temp
#include "arm.h"

#define ARMLOCAL    (VIRTIO+IOSIZE)

enum {
    Localctl    = 0x00,
    Prescaler   = 0x08,
};

extern uvlong tmrget(void);

void test(void) {

    *(ulong*)(ARMLOCAL + Localctl) = 0;             /* magic */
    *(ulong*)(ARMLOCAL + Prescaler) = 0x06aaaaab;   /* magic for 1 Mhz */

    ulong x, y;

    x = *(ulong*)(ARMLOCAL + 0x1C);
    y = *(ulong*)(ARMLOCAL + 0x20);
    print("%d %d\n", x, y); // does not work, get 0 0

}

void test2(void) {
    ulong x;
    uvlong v;

    x = cprdsc(0, CpTIMER, 0, 0);
    print("freq = %d\n", x);

    *(ulong*)(ARMLOCAL + Localctl) = 0;             /* magic */
    *(ulong*)(ARMLOCAL + Prescaler) = 0x06aaaaab;   /* magic for 1 Mhz */

    arch_coherence();

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
