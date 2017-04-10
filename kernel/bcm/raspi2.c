/*s: init/arm/raspi2.c */
/*
 * bcm2836 (e.g.raspberry pi 2) architecture-specific stuff
 */
/*s: kernel basic includes */
#include <u.h>
#include "port/lib.h"
#include "port/error.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
/*e: kernel basic includes */

#include "arm.h"

typedef struct Mbox Mbox;
typedef struct Mboxes Mboxes;

/*s: constant ARMLOCAL(arm) */
#define ARMLOCAL    (VIRTIO+IOSIZE)
/*e: constant ARMLOCAL(arm) */

/*s: global soc(raspberry pi2)(arm) */
Soc soc = {
    .dramsize   = 1024*MiB,
    .physio     = 0x3F000000,
     /*s: [[soc(raspberry pi2)]] other fields(arm) */
     .l1ptedramattrs = Cached | Buffered | L1wralloc | L1sharable,
     .l2ptedramattrs = Cached | Buffered | L2wralloc | L2sharable,
     /*x: [[soc(raspberry pi2)]] other fields(arm) */
     .armlocal   = 0x40000000,
     /*x: [[soc(raspberry pi2)]] other fields(arm) */
     .busdram    = 0xC0000000,
     /*x: [[soc(raspberry pi2)]] other fields(arm) */
     .busio      = 0x7E000000,
     /*e: [[soc(raspberry pi2)]] other fields(arm) */
};
/*e: global soc(raspberry pi2)(arm) */


/*s: struct Mbox(arm) */
/*
 * Arm local regs for SMP
 */
struct Mbox {
    u32int  doorbell;
    u32int  mbox1;
    u32int  mbox2;
    u32int  startcpu;
};
/*e: struct Mbox(arm) */
/*s: struct Mboxes(arm) */
struct Mboxes {
    Mbox    set[4];
    Mbox    clr[4];
};
/*e: struct Mboxes(arm) */

enum {
    Mboxregs    = 0x80
};

static Lock startlock[MAXCPUS + 1];


/*s: function l2ap(raspberry pi2)(arm) */
int
l2ap(int ap)
{
    return (AP(0, (ap)));
}
/*e: function l2ap(raspberry pi2)(arm) */

/*e: init/arm/raspi2.c */
