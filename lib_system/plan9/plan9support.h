
#define Nothing ((value) 0)

extern void plan9_error (int errcode, char * cmdname, value arg) Noreturn;
extern void p9error (char * cmdname, value arg) Noreturn;
