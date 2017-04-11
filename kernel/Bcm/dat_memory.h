
/*
 *  MMU stuff in proc
 */
#define NCOLOR  1       /* 1 level cache, don't worry about VCE's */

struct Arch_Proc
{
    // list<ref<Page> > pages used to store page tables
    Page*   mmul2;
    // list<ref<Page> > (next = Page.next)
    Page*   mmul2cache; /* free mmu pages */
};

/*
 * Fake kmap.
 */
typedef void        Arch_KMap;
#define VA(k)       ((uintptr)(k))

