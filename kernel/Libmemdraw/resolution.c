#include <u.h>
#include <libc.h>
#include <draw.h>
#include <memdraw.h>

byte*
byteaddr(Memimage *i, Point p)
{
    byte *a;

    a = (i->data->bdata + i->zero) + (sizeof(ulong) * p.y * i->width);

    if(i->depth < 8){
        /*
         * We need to always round down,
         * but C rounds toward zero.
         */
        int np;
        np = 8/i->depth;
        if(p.x < 0)
            return a+(p.x-np+1)/np;
        else
            return a+p.x/np;
    }
    else
        return a + p.x * (i->depth/8);
}

/*
 * Wordaddr is deprecated.
 */
ulong*
wordaddr(Memimage *i, Point p)
{
    return (ulong*) ((uintptr)byteaddr(i, p) & ~(sizeof(ulong)-1));
}

ulong
pixelbits(Memimage *i, Point pt)
{
    byte *p;
    ulong val = 0;
    int bpp = i->depth; // bits per pixel
    int off, npack;

    p = byteaddr(i, pt);

    switch(bpp){
    case 1:
    case 2:
    case 4:
        npack = 8/bpp;
        off = pt.x % npack;
        val = p[0] >> bpp*(npack-1-off);
        val &= (1<<bpp)-1;
        break;
    case 16:
        val = p[0]|(p[1]<<8);
        break;
    case 24:
        val = p[0]|(p[1]<<8)|(p[2]<<16);
        break;
    case 8:
        val = p[0];
        break;
    case 32:
        val = p[0]|(p[1]<<8)|(p[2]<<16)|(p[3]<<24);
        break;
    }
    // duplicate byte in the whole word
    while(bpp<32){
        val |= val<<bpp;
        bpp *= 2;
    }
    return val;
}

