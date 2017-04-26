#include "config.h"


// when don't ave APE, have to fake those for now.

int errno = -1;

int myopen(char*, int, int) { return -1; }

int unlink(char*)           { return -1;}
int rename(char*, char*)    { return -1; }
int getcwd(char*, int)      { return 0; }
int system(char*)           { return -1; }

char * strerror(int n)      { return "ERROR"; }

int sscanf(const char *, const char *, ...) { return -1; }

void signal(int, void (*)()) {
}

// for scheduler.c
extern tmrget(unsigned long long *x);
double timeofday(void)
{
  //struct timeval tv;
  //gettimeofday(&tv, NULL);
  //return (double) tv.tv_sec + (double) tv.tv_usec * 1e-6;
   unsigned long long x;
   tmrget(&x);
   return (double) x / 62500000.0;
}
