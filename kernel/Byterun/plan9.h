
// if use APE
#undef __GNUC__
#undef ARCH_SIXTYFOUR
#undef HAS_TERMCAP
#undef HAS_SETITIMER

// if dont use APE, also have to define and undef the following
#define OS_PLAN9

#define NULL 0
#undef POSIX_SIGNALS

#undef HAS_TERMCAP
#undef HAS_SOCKETS
#undef HAS_UNISTD

#undef HAS_DIRENT
#undef HAS_REWINDDIR
#undef HAS_LOCKF
#undef HAS_MKFIFO
#undef HAS_GETCWD
#undef HAS_GETWD
#undef HAS_GETPRIORITY
#undef HAS_UTIME
#undef HAS_UTIMES
#undef HAS_DUP2
#undef HAS_FCHMOD
#undef HAS_TRUNCATE
#undef HAS_SELECT
#undef HAS_SYS_SELECT_H
#undef HAS_SYMLINK
#undef HAS_WAITPID
#undef HAS_WAIT4
#undef HAS_GETGROUPS
#undef HAS_TERMIOS
#undef HAS_ASYNC_IO
#undef HAS_SETITIMER
#undef HAS_GETHOSTNAME
#undef HAS_UNAME
#undef HAS_GETTIMEOFDAY
#undef HAS_MKTIME
#undef HAS_SETSID

#include <u.h>
#include <libc.h>

#define bcopy(src,dst,len) memmove((dst), (src), (len))
#define sprintf sprint
#define fprintf fprint

#define stderr STDERR

#define fflush(x) USED(x)
#define exit(x) _exits(0)

#define lseek(a,b,c) seek(a,b,c)
#define SEEK_END SEEK__END
#define SEEK_SET SEEK__START


#define INT_MAX		0x7fffffff
#define LONG_MAX	0x7fffffffL

#define EINTR -1
#define EAGAIN -1

#define O_RDONLY OREAD
#define O_WRONLY OWRITE
#define O_TRUNC OTRUNC
#define O_EXCL OEXCL
#define O_APPEND -1
#define O_CREAT -1
#define O_NONBLOCK -1

typedef unsigned long size_t;

int unlink(char*);
int rename(char*, char*);
int getcwd(char*, int);
int system(char*);
int errno;
int myopen(char*, int, int);

extern int sscanf(const char *, const char *, ...);

void signal(int, void (*)());

#define SIG_DFL ((void (*)())0)
#define SIG_ERR ((void (*)())-1)
#define SIG_IGN ((void (*)())1)
