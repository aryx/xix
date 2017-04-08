/*s: memory/arm/mmu.c */
/*s: kernel basic includes */
#include <u.h>
#include "port/lib.h"
#include "port/error.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
/*e: kernel basic includes */
#include "arm.h"

/*s: macro L1X(arm) */
#define L1X(va)     FEXT((va), 20, 12)
/*e: macro L1X(arm) */
/*s: macro L2X(arm) */
#define L2X(va)     FEXT((va), 12, 8)
/*e: macro L2X(arm) */
/*s: macro L2AP(arm) */
#define L2AP(ap)    l2ap(ap)
/*e: macro L2AP(arm) */

/*s: constant L1ptedramattrs(arm) */
#define L1ptedramattrs  soc.l1ptedramattrs
/*e: constant L1ptedramattrs(arm) */
/*s: constant L2ptedramattrs(arm) */
#define L2ptedramattrs  soc.l2ptedramattrs
/*e: constant L2ptedramattrs(arm) */

/*s: enum _anon_ (memory/arm/mmu.c)(arm) */
enum {
/*s: constant L1lo(arm) */
L1lo        = UZERO/MiB,        /* L1X(UZERO)? */
/*e: constant L1lo(arm) */
/*s: constant L1hi(arm) */
L1hi        = (USTKTOP+MiB-1)/MiB,  /* L1X(USTKTOP+MiB-1)? */
/*e: constant L1hi(arm) */
};
/*e: enum _anon_ (memory/arm/mmu.c)(arm) */

/*s: function mmuinit(arm) */
/*
 * Set up initial PTEs for this cpu (called with mmu off)
 */
void
mmuinit(void *a)
{
    PTE *l1, *l2;
    phys_addr pa;
    kern_addr va;

    l1 = (PTE*)a; // PADDR(L1) for cpu0

    /*
     * map all of ram at KZERO
     */
    va = KZERO;
    for(pa = 0; pa < soc.dramsize; pa += MiB, va += MiB)
    {
        l1[L1X(va)] = pa|Dom0|L1AP(Krw)|Section|L1ptedramattrs;
    }

    /*
     * identity map first MB of ram so mmu can be enabled
     */
    // ???
    l1[L1X(0)] = 0|Dom0|L1AP(Krw)|Section|L1ptedramattrs;

    /*
     * map i/o registers 
     */
    va = VIRTIO;
    for(pa = soc.physio; pa < soc.physio + IOSIZE; pa += MiB, va += MiB){
        // No L1ptedramattrs (Cached | Buffered) here, because volatile!
        l1[L1X(va)] = pa|Dom0|L1AP(Krw)|Section; 
    }
    // for raspi2
    pa = soc.armlocal;
    if(pa)
        l1[L1X(va)] = pa|Dom0|L1AP(Krw)|Section;
    
    /*
     * double map exception vectors at top of virtual memory
     */
    // ???
    l2 = (PTE*)PADDR(L2);
    va = HVECTORS;
    l1[L1X(va)] = (uintptr)l2|Dom0|Coarse;
    l2[L2X(va)] = L2AP(Krw)|Small|L2ptedramattrs;
}
/*e: function mmuinit(arm) */

/*s: function mmuinit1(arm) */
void
mmuinit1(void *a)
{
    PTE *l1;

    l1 = (PTE*)a;
    cpu->mmul1 = l1;

    /*
     * undo identity map of first MB of ram
     */
    l1[L1X(0)] = 0;
    /*s: [[mmuinit1()]] write back cache after adjusting page tables(arm) */
    //cachedwbse(&l1[L1X(0)], sizeof(PTE));
    /*e: [[mmuinit1()]] write back cache after adjusting page tables(arm) */
    mmuinvalidateaddr(0);
}
/*e: function mmuinit1(arm) */


/*s: function arch_cankaddr(arm) */
/*
 * Return the number of bytes that can be accessed via KADDR(pa).
 * If pa is not a valid argument to KADDR, return 0.
 */
uintptr
arch_cankaddr(uintptr pa)
{
    if(pa < memsize)        /* assumes PHYSDRAM is 0 */
        return memsize - pa;
    return 0;
}
/*e: function arch_cankaddr(arm) */

/*s: function mmukmap(arm) */
uintptr
mmukmap(uintptr va, uintptr pa, usize size)
{
    int o;
    usize n;
    PTE *pte, *pte0;

    assert((va & (MiB-1)) == 0);
    o = pa & (MiB-1);
    pa -= o;
    size += o;
    pte = pte0 = &cpu->mmul1[L1X(va)];
    for(n = 0; n < size; n += MiB)
        if(*pte++ != Fault)
            return 0;
    pte = pte0;
    for(n = 0; n < size; n += MiB){
        *pte++ = (pa+n)|Dom0|L1AP(Krw)|Section;
        mmuinvalidateaddr(va+n);
    }
    /*s: [[mmukmap()]] write back cache after changing page tables(arm) */
    //cachedwbse(pte0, (uintptr)pte - (uintptr)pte0);
    /*e: [[mmukmap()]] write back cache after changing page tables(arm) */
    return va + o;
}
/*e: function mmukmap(arm) */
