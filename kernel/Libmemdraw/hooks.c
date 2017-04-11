#include <u.h>
#include <libc.h>
#include <draw.h>
#include <memdraw.h>

int
hwdraw_OVERRIDEN(Memdrawparam*)
{
    return 0;	/* could not satisfy request */
}

int
memdraw_iprint(char*,...)
{
    return -1;
}

int		(*iprint)(char*, ...) = &memdraw_iprint;

