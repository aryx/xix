
#ifndef OS_PLAN9
// from syscall.h
#define	ERRMAX	128
#endif

#define Nothing ((value) 0)

extern void plan9_error (char * cmdname, value arg) Noreturn;
extern void p9error (char * cmdname) Noreturn;

#ifdef OS_PLAN9_APE
#include <lib9.h>
#endif
