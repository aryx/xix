
// from syscall.h
#define	ERRMAX	128

#define Nothing ((value) 0)

extern void plan9_error (char * cmdname, value arg) Noreturn;
extern void p9error (char * cmdname) Noreturn;
