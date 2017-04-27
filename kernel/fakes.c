#include <u.h>
#include "port/lib.h"
#include "port/error.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"

// Fake for the kernel

char* getconf(char *) { return nil; }

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
char* getenv(char* x1, char*, int) {
  panic("getenv: link = %lux", getcallerpc(&x1));
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
