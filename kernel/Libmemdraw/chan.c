#include <u.h>
#include <libc.h>
#include <draw.h>
#include <memdraw.h>

errorneg1
memsetchan(Memimage *i, ulong chan)
{
    int d;
    int t, j, k;
    ulong cc;
    bool bytes;

    d = chantodepth(chan);
    if(d == 0) {
        werrstr("bad channel descriptor");
        return ERROR_NEG1;
    }
    i->chan = chan;
    i->depth = d;
    i->flags &= ~(Fgrey|Falpha|Fcmap|Fbytes);
    bytes = true;

    for(cc=chan, j=0, k=0; cc; j+=NBITS(cc), cc>>=8, k++){
        t=TYPE(cc);
        if(t < 0 || t >= NChan){
            werrstr("bad channel string");
            return -1;
        }
        if(t == CGrey)
            i->flags |= Fgrey;
        if(t == CAlpha)
            i->flags |= Falpha;
        if(t == CMap && i->cmap == nil){
            i->cmap = memdefcmap;
            i->flags |= Fcmap;
        }

        i->shift[t] = j;
        i->mask[t] = (1<<NBITS(cc))-1;
        i->nbits[t] = NBITS(cc);

        if(NBITS(cc) != 8)
            bytes = false;
    }
    i->nchan = k;
    if(bytes)
        i->flags |= Fbytes;
    return OK_0;
}

