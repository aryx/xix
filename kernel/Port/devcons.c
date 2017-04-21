#include    "u.h"
#include    "../port/lib.h"
#include    "../port/error.h"
#include    "mem.h"
#include    "dat.h"
#include    "fns.h"

// Simplified versions!! The originals are in comments before.

//void
//putstrn(char *str, int n)
//{
//    putstrn0(str, n, false);
//}


void
putstrn(char *str, int n)
{
    screenputs(str, n);
}



//bool noprint; // to debug?
//
//int
//devcons_print(char *fmt, ...)
//{
//    int n;
//    va_list arg;
//    char buf[PRINTSIZE];
//
//    if(noprint)
//        return -1;
//
//    va_start(arg, fmt);
//    n = vseprint(buf, buf+sizeof(buf), fmt, arg) - buf;
//    va_end(arg);
//    putstrn(buf, n);
//
//    return n;
//}

int
devcons_print(char *fmt, ...)
{
    int n;
    va_list arg;
    char buf[256];

    va_start(arg, fmt);
    n = vseprint(buf, buf+sizeof(buf), fmt, arg) - buf;
    va_end(arg);
    putstrn(buf, n);

    return n;
}



///*
// * Want to interlock iprints to avoid interlaced output on 
// * multiprocessor, but don't want to deadlock if one processor
// * dies during print and another has something important to say.
// * Make a good faith effort.
// */
//static Lock iprintlock;
//
//static bool
//iprintcanlock(Lock *l)
//{
//    int i;
//    
//    for(i=0; i<1000; i++){
//        if(canlock(l))
//            return true;
//        if(l->cpu == CPUS(cpu->cpuno))
//            return false;
//        arch_microdelay(100);
//    }
//    return false;
//}
//
//int
//devcons_iprint(char *fmt, ...)
//{
//    int n, s, locked;
//    va_list arg;
//    char buf[PRINTSIZE];
//
//    s = arch_splhi();
//
//    va_start(arg, fmt);
//    n = vseprint(buf, buf+sizeof(buf), fmt, arg) - buf;
//    va_end(arg);
//
//    locked = iprintcanlock(&iprintlock);
//
//    kmesgputs(buf, n); // addon
//    if(screenputs != nil && iprintscreenputs)
//        screenputs(buf, n);
//    uartputs(buf, n);
//
//    if(locked)
//        unlock(&iprintlock);
//
//    arch_splx(s);
//
//    return n;
//}


int
devcons_iprint(char *fmt, ...)
{
    int n;
    va_list arg;
    char buf[256];

    va_start(arg, fmt);
    n = vseprint(buf, buf+sizeof(buf), fmt, arg) - buf;
    va_end(arg);
    putstrn(buf, n);

    return n;
}



//void
//devcons_panic(char *fmt, ...)
//{
//    int n, s;
//    va_list arg;
//    char buf[PRINTSIZE];
//
//    /*s: [[panic()]] reset kprintoq */
//    kprintoq = nil; /* don't try to write to /dev/kprint */
//    /*e: [[panic()]] reset kprintoq */
//
//    if(panicking)
//        for(;;);
//    panicking = true;
//
//    s = arch_splhi();
//    strcpy(buf, "panic: ");
//    va_start(arg, fmt);
//    n = vseprint(buf+strlen(buf), buf+sizeof(buf), fmt, arg) - buf;
//    va_end(arg);
//    iprint("%s\n", buf);
//    /*s: [[panic()]] run consdebug hook */
//    if(consdebug)
//        (*consdebug)();
//    /*e: [[panic()]] run consdebug hook */
//    arch_splx(s);
//    prflush();
//    buf[n] = '\n';
//    putstrn(buf, n+1);
//    //TODO: put in comment for now because already got some panic with
//    // lapic, but it seems to work still :)
//    //arch_dumpstack();
//    //exit(1);
//}

void devcons_panic(char *fmt, ...)
{
    int n;
    va_list arg;
    char buf[256];

    putstrn("panic: ", 7);
    va_start(arg, fmt);
    n = vseprint(buf, buf+sizeof(buf), fmt, arg) - buf;
    va_end(arg);
    putstrn(buf, n);
    putstrn("\n", 1);
    for(;;) { }
}

