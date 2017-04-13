/*
 * bcm2836 (e.g.raspberry pi 2) architecture-specific stuff
 */
#include <u.h>
#include "port/lib.h"
#include "port/error.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"

#include "arm.h"

typedef struct Mbox Mbox;
typedef struct Mboxes Mboxes;

#define ARMLOCAL    (VIRTIO+IOSIZE)

Soc soc = {
    .dramsize   = 1024*MiB,
    .physio     = 0x3F000000,
    .armlocal   = 0x40000000,
    .busdram    = 0xC0000000,
    .busio      = 0x7E000000,
};


/*
 * Arm local regs for SMP
 */
struct Mbox {
    u32int  doorbell;
    u32int  mbox1;
    u32int  mbox2;
    u32int  startcpu;
};
struct Mboxes {
    Mbox    set[4];
    Mbox    clr[4];
};

enum {
    Mboxregs    = 0x80
};

static Lock startlock[MAXCPUS + 1];


int
l2ap(int ap)
{
    return (AP(0, (ap)));
}
