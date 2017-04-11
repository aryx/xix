#include <u.h>
#include <libc.h>
#include <draw.h>
#include <memdraw.h>
#include <pool.h>

extern Pool* imagmem;

static Memimage*	memones;
static Memimage*	memzeros;
Memimage *memwhite;
Memimage *memblack;
Memimage *memtransparent;
Memimage *memopaque;

int	_ifmt(Fmt*);

extern int replmul[];

extern uchar replbit[1+8][256];

static void
mktables(void)
{
    int i, j, small;
    static bool	tablesbuilt = false;
    if(tablesbuilt)
        return;
    tablesbuilt = true;

    /* bit replication up to 8 bits */
    for(i=0; i<256; i++){
        for(j=0; j<=8; j++){	/* j <= 8 [sic] */
            small = i & ((1<<j)-1);
            replbit[j][i] = (small*replmul[j])>>8;
        }
    }

}


void
memimageinit(void)
{
    static bool didinit = false;
    if(didinit)
        return;
    didinit = true;

    if(  strcmp(imagmem->name, "Image") == 0 
      || strcmp(imagmem->name, "image") == 0
      )
        imagmem->move = memimagemove;
    _memmkcmap();
    mktables();
    fmtinstall('P', Pfmt);
    fmtinstall('R', Rfmt); 
    fmtinstall('b', _ifmt);

    memzeros = allocmemimage(Rect(0,0,1,1), GREY1);
    memzeros->flags |= Frepl;
    memzeros->clipr = Rect(-0x3FFFFFF, -0x3FFFFFF, 0x3FFFFFF, 0x3FFFFFF);
    *byteaddr(memzeros, ZP) = 0;

    memones = allocmemimage(Rect(0,0,1,1), GREY1);
    memones->flags |= Frepl;
    memones->clipr = Rect(-0x3FFFFFF, -0x3FFFFFF, 0x3FFFFFF, 0x3FFFFFF);
    *byteaddr(memones, ZP) = ~0;

    if(memones == nil || memzeros == nil)
        assert(0 /*cannot initialize memimage library */);	/* RSC BUG */

    memwhite = memones;
    memblack = memzeros;
    memopaque = memones;
    memtransparent = memzeros;
}

