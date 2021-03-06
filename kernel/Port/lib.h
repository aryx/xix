// coupling: libc.h
// This file mostly exports code from lib_core/libc/ (linked with the kernel).
// Those functions are also exported in include/libc.h but they prefered to
// not include it and instead to reexport here a subset specific to the kernel.
// There are also the poolxxx() functions exported in include/pool.h
// (also part of libc) that are used in memory/alloc.c.
// Some functions in libc are also "overriden" (via linker abuse):
//  - print.c overrides _fmtlock(), _fmtunlock() that were in libc/fmt/
//  - devcons.s overrides the sysfatal() that was in libc/9sys/

// pad specific, could be in u.h
typedef int bool;
enum _bool {
  false = 0,
  true = 1
};

typedef uchar byte;

typedef ushort bool_ushort; //TODO: delete

typedef int fdt; // file descriptor type

#define OK_0 0
#define OK_1 1
#define ERROR_0 0
#define ERROR_1 1
#define ERROR_NEG1 (-1)
typedef int error0;
typedef int error1;
typedef int errorneg1;

//#define nil (void*)0 in u.h
#define nilptr 0

typedef struct Qid  Qid;
typedef struct DirEntry  DirEntry;
typedef struct DirEntry  Dir; // for fcall.h
typedef struct Waitmsg  Waitmsg;
typedef struct Fmt  Fmt;

/*
 * functions (mostly) linked in from libc.
 */
#define nelem(x)  (sizeof(x)/sizeof((x)[0]))
//@Scheck: not dead, actually exanded in macros.h
#define offsetof(s, m)  (ulong)(&(((s*)0)->m))
#define assert(x) do { if(x) {}else _assert("x");}while(0)

/*
 * mem routines
 */
extern  void* memccpy(void*, void*, int, ulong);
extern  void* memset(void*, int, ulong);
extern  int memcmp(void*, void*, ulong);
extern  void* memmove(void*, void*, ulong);
extern  void* memchr(void*, int, ulong);

/*
 * string routines
 */
extern  char* strchr(char*, int);
extern  char* strrchr(char*, int);
extern  int strcmp(char*, char*);
extern  char* strcpy(char*, char*);
extern  char* strecpy(char*, char*, char*);
extern  char* strncpy(char*, char*, long);
extern  int strncmp(char*, char*, long);
extern  long  strlen(char*);
extern  char* strstr(char*, char*);
extern  int atoi(char*);
extern  int fullrune(char*, int);

//redefined in the kernel
extern  int cistrcmp(char*, char*);
extern  int cistrncmp(char*, char*, int);

enum
{
  UTFmax    = 4,    /* maximum bytes per rune */
  Runeself  = 0x80,   /* rune and UTF sequences are the same (<) */
};

/*
 * rune routines
 */
extern  int runetochar(char*, Rune*);
extern  int chartorune(Rune*, char*);
extern  char* utfrune(char*, long);
extern  int utfnlen(char*, long);

extern  int abs(int);

/*
 * print routines
 */
typedef int (*Fmts)(Fmt*);
struct Fmt{
  byte runes;      /* output buffer is runes or chars? */
  void  *start;     /* of buffer */
  void  *to;      /* current place in the buffer */
  void  *stop;      /* end of the buffer; overwritten if flush fails */
  int (*flush)(Fmt *);  /* called when to == stop */
  void  *farg;      /* to make flush a closure */
  int nfmt;     /* num chars formatted so far */
  va_list args;     /* args passed to dofmt */
  int r;      /* % format Rune */
  int width;
  int prec;
  ulong flags;
};

// This used to be regular function, but to avoid backward deps in the kernel
// I made it into a pointer function (a bit ugly, and maybe unsafe)
extern  int (*print)(char*, ...);

extern  char* seprint(char*, char*, char*, ...);
extern  char* vseprint(char*, char*, char*, va_list);
extern  int snprint(char*, int, char*, ...);
extern  int sprint(char*, char*, ...);

#pragma varargck  argpos  fmtprint  2
#pragma varargck  argpos  print   1
#pragma varargck  argpos  seprint   3
#pragma varargck  argpos  snprint   3
#pragma varargck  argpos  sprint    2

#pragma varargck  type  "lld" vlong
#pragma varargck  type  "llx" vlong
#pragma varargck  type  "lld" uvlong
#pragma varargck  type  "llx" uvlong
#pragma varargck  type  "ld"  long
#pragma varargck  type  "lx"  long
#pragma varargck  type  "ld"  ulong
#pragma varargck  type  "lx"  ulong
#pragma varargck  type  "d" int
#pragma varargck  type  "x" int
#pragma varargck  type  "c" int
#pragma varargck  type  "C" int
#pragma varargck  type  "d" uint
#pragma varargck  type  "x" uint
#pragma varargck  type  "c" uint
#pragma varargck  type  "C" uint
#pragma varargck  type  "s" char*
#pragma varargck  type  "q" char*
#pragma varargck  type  "S" Rune*
#pragma varargck  type  "%" void
#pragma varargck  type  "p" uintptr
#pragma varargck  type  "p" void*
#pragma varargck  flag  ','

extern  int fmtstrinit(Fmt*);
extern  int fmtinstall(int, int (*)(Fmt*));
extern  void  quotefmtinstall(void);
extern  int fmtprint(Fmt*, char*, ...);
extern  int fmtstrcpy(Fmt*, char*);
extern  char* fmtstrflush(Fmt*);

/*
 * one-of-a-kind
 */
extern  long  strtol(char*, char**, int);
extern  ulong strtoul(char*, char**, int);
extern  vlong strtoll(char*, char**, int);
extern  uvlong  strtoull(char*, char**, int);

//todo: should return a (void*)
extern  ulong getcallerpc(void*);
extern  char* cleanname(char*);
extern  int getfields(char*, char**, int, int, char*);
extern  int tokenize(char*, char**, int);
extern  void  qsort(void*, long, long, int (*)(void*, void*));

extern  char  etext[];
//@Scheck: Assembly, not dead used by 386/l.s and arm/main.c
extern  char  edata[];
extern  char  end[];


/*
 * Syscall data structures
 */
enum MountFlags {
    MREPL = 0x0000,  /* mount replaces object */
  
    MBEFORE = 0x0001,  /* mount goes before others in union directory */
    MAFTER = 0x0002,  /* mount goes after others in union directory */
  
    MCREATE = 0x0004,  /* permit creation in mounted directory */
    MCACHE = 0x0010,  /* cache some data */
  
    MORDERMASK =  0x0003,  /* mask for bits defining order of mounting */
    MMASK = 0x0017,  /* all bits on */
};

enum OpenFlags {
    OREAD = 0, /* open for read */
    OWRITE = 1, /* write */
    ORDWR = 2, /* read and write */
    OEXEC = 3, /* execute, == read but check execute permission */
    OCEXEC = 32,  /* or'ed in, close on exec */
    ORCLOSE = 64,  /* or'ed in, remove on close */
    OTRUNC = 16,  /* or'ed in (except for exec), truncate file first */
    OEXCL = 0x1000,  /* or'ed in, exclusive create */
};

enum NoteHook {
  NCONT = 0, /* continue after note */
  NDFLT = 1, /* terminate after note */
  NSAVE = 2, /* clear note but hold state */
  NRSTR = 3, /* restore saved state */
};

enum Miscsize {  
    ERRMAX = 128, /* max length of error string */
    KNAMELEN = 28,  /* max length of name held in kernel */
};



/* bits in Qid.type */
enum Qidtype {
  QTFILE = 0x00,    /* plain file */
  QTDIR = 0x80,    /* type bit for directories */
  QTMOUNT = 0x10,    /* type bit for mounted channel */
  QTAUTH = 0x08,    /* type bit for authentication file */
  QTAPPEND = 0x40,    /* type bit for append only files */
  QTEXCL = 0x20,    /* type bit for exclusive use files */
};

/* bits in DirEntry.mode */
enum Dirmode {
    DMDIR = 0x80000000,  /* mode bit for directories */

    DMREAD = 0x4,   /* mode bit for read permission */
    DMWRITE = 0x2,   /* mode bit for write permission */
    DMEXEC = 0x1,   /* mode bit for execute permission */
    DMMOUNT = 0x10000000,  /* mode bit for mounted channel */
    DMEXCL = 0x20000000,  /* mode bit for exclusive use files */
};

struct Qid
{
  // note that this is not a string, but an int! it's kind of an inode
  uvlong  path;
  // for cache invalidation
  ulong vers;
  // enum<Qidtype>
  byte type;
};

struct DirEntry {
  /* system-modified data */
  ushort  type; /* server type */
  uint  dev;  /* server subtype */

  /* file data */
  Qid qid;  /* unique id from server */
  // bitset<enum<dirmode>>
  ulong mode; /* permissions */

  ulong atime;  /* last read time */
  ulong mtime;  /* last write time */

  vlong length; /* file length: see <u.h> */
  char  *name;  /* last element of path */

  char  *uid; /* owner name */
  char  *gid; /* group name */
  char  *muid;  /* last modifier name */
};


struct Waitmsg
{
  int pid;    /* of loved one */ // pid of the child
  ulong time[3];  /* of loved one and descendants */
  char  msg[ERRMAX];  /* actually variable-size in user mode */
};

