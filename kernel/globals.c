#include <u.h>
#include "port/lib.h"
#include "port/error.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"

int (*print)(char*, ...) = nil;
void (*_assert)(char *fmt) = nil;

// usually used as default callback for sleep/tsleep
bool
returnfalse(void*)
{
    return false;
}

Conf conf;
char *confname[MAXCONF];
char *confval[MAXCONF];
int nconf;

Cpu* cpus[MAXCPUS];

// for xinit
struct Palloc palloc;
