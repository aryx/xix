#include    "u.h"
#include    "../port/lib.h"
#include    "../port/error.h"
#include    "mem.h"
#include    "dat.h"
#include    "fns.h"

#include    <draw.h>
#include    <memdraw.h>
#include    <memlayer.h>
#include    <cursor.h>

// Many of the stuff below used to be in vgascreen.c, but they are
// quite VGA independent so better to have a generic portscreen.c.

Memimage *gscreen;
Memdata gscreendata;
Rectangle physgscreenr;

// for software cursor look swcursor.c
// for software console look swconsole.c

