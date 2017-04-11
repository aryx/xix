#include <u.h>
#include <libc.h>
#include <draw.h>
#include <memdraw.h>
#include <memlayer.h>

Memimage*
memlalloc(Memscreen *s, Rectangle screenr, Refreshfn refreshfn, void *refreshptr, rgba val)
{
    Memimage *n;
    Memlayer *l;
    static Memimage *paint;

    if(paint == nil){
        paint = allocmemimage(Rect(0,0,1,1), RGBA32);
        if(paint == nil)
            return nil;
        paint->flags |= Frepl;
        paint->clipr = Rect(-0x3FFFFFF, -0x3FFFFFF, 0x3FFFFFF, 0x3FFFFFF);
    }

    // share pixels data with screen image
    n = allocmemimaged(screenr, s->image->chan, s->image->data);
    if(n == nil)
        return nil;
    // overwrite zero and width derived from screenr in allocmemimaged
    // because the rectangle here does not match the data, it's a
    // subrectangle. Zero and width should remain the same.
    n->zero = s->image->zero;
    n->width = s->image->width;

    l = malloc(sizeof(Memlayer));
    if(l == nil){
        free(n);
        return nil;
    }

    l->screen = s;
    l->screenr = screenr;
    l->delta = Pt(0,0); // can be changed later by originwindow()
    l->clear = false;
    if(refreshfn)
        l->save = nil;
    else{
        l->save = allocmemimage(screenr, s->image->chan);
        if(l->save == nil){
            free(l);
            free(n);
            return nil;
        }
        /* allocmemimage doesn't initialize memory; this paints save area */
        if(val != DNofill)
            memfillcolor(l->save, val);
    }

    n->layer = l;
    // ok no more sanity check, we can make the connection
    n->data->ref++;

    l->refreshfn = refreshfn;
    l->refreshptr = nil;	/* don't set it until we're done */
    /* start with new window behind all existing ones */

    // add_end_double_list(l, s->frontmost, s->rearmost)
    l->front = s->rearmost;
    l->rear = nil;
    if(s->rearmost)
        s->rearmost->layer->rear = n;
    s->rearmost = n;
    if(s->frontmost == nil)
        s->frontmost = n;

    /* now pull new window to front */
    _memltofrontfill(n, val != DNofill);
    l->refreshptr = refreshptr;

    /*
     * paint with requested color; previously exposed areas are already right
     * if this window has backing store, but just painting the whole thing is simplest.
     */
    if(val != DNofill){
        memsetchan(paint, n->chan);
        memfillcolor(paint, val);
        memdraw(n, n->r, paint, n->r.min, nil, n->r.min, S);
    }
    return n;
}
