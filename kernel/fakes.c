#include <u.h>
#include "port/lib.h"
#include "port/error.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"

// Fake for the kernel

char* getconf(char *) { return nil; }

Timer*
addclock0link(void (*f)(void), Tms ms) { USED(f); USED(ms); return nil; }

// used in debug code for EDF support in lock()
vlong todget(vlong *) { 
  panic("todget"); 
  return 0; 
}

// Fake for ocamlrun

long write(fdt fd, void*p, long n) {
  if(fd == 1 || fd == 2) {
    print((char*)p);
  } else {
    panic("write");
  }
  return n;
}

long read(fdt, void*, long) {
  panic("read");
  return 0;
}

void	abort(void) {
  panic("abort");
}

void	werrstr(char*, ...) {
  panic("werrstr");
}
void	rerrstr(char*, uint) {
  panic("rerrstr");
}

void	_exits(char*) {
  panic("_exits");
}
//char*   getenv(char*) {
char* getenv(char*, char*, int) {
  panic("getenv");
  return 0;
}
fdt		open(char*, int) {
  panic("open");
  return 0;
}
int		close(fdt) {
  panic("close");
  return 0;
}
vlong	seek(fdt, vlong, int) {
  panic("seek");
  return 0;
}
int		chdir(char*) {
  panic("chdir");
  return 0;
}
