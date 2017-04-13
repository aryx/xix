
/* flag values */
enum BlockFlags
{
  BINTR = (1<<0),
  BFREE = (1<<1),
  Bipck = (1<<2),   /* ip checksum */
  Budpck  = (1<<3),   /* udp checksum */
  Btcpck  = (1<<4),   /* tcp checksum */
  Bpktck  = (1<<5),   /* packet checksum */
};

struct Block
{
  long  ref;
  Block*  next;
  Block*  list;
  byte*  rp;     /* first unconsumed byte */
  byte*  wp;     /* first empty byte */
  byte*  lim;      /* 1 past the end of the buffer */
  byte*  base;     /* start of the buffer */
  void  (*free)(Block*);
  ushort  flag;
  ushort  checksum;   /* IP checksum of complete packet (minus media header) */
};

#define BLEN(s) ((s)->wp - (s)->rp)
#define BALLOC(s) ((s)->lim - (s)->base)


/* queue state bits,  Qmsg, Qcoalesce, and Qkick can be set in qopen */
enum QueueState
{
  /* Queue.state */
  Qstarve   = (1<<0), /* consumer starved */
  Qmsg    = (1<<1), /* message stream */
  Qclosed   = (1<<2), /* queue has been closed/hungup */
  Qflow   = (1<<3), /* producer flow controlled */
  Qcoalesce = (1<<4), /* coalesce packets on read */
  Qkick   = (1<<5), /* always call the kick routine after qwrite */
};

// defined in qio.c
extern  uint  qiomaxatomic;

/*
 *  IO queues
 */
// was in qio.c
struct Queue
{
  Lock;

  Block*  bfirst;   /* buffer */
  Block*  blast;

  int len;    /* bytes allocated to queue */
  int dlen;   /* data bytes in queue */
  int limit;    /* max bytes in queue */
  int inilim;   /* initial limit */
  int state;
  int noblock;  /* true if writes return immediately when q full */
  int eof;    /* number of eofs read by user */

  void  (*kick)(void*); /* restart output */
  void  (*bypass)(void*, Block*); /* bypass queue altogether */
  void* arg;    /* argument to kick */

  QLock rlock;    /* mutex for reading processes */
  Rendez  rr;   /* process waiting to read */
  QLock wlock;    /* mutex for writing processes */
  Rendez  wr;   /* process waiting to write */

  char  err[ERRMAX];
};


// was in cache.c
struct Extent
{
  int bid;
  ulong start;
  int len;
  Page  *cache;
  Extent  *next;
};

// was in cache.c
struct Mntcache
{
  Qid qid;
  int dev;
  int type;
  QLock;
  Extent   *list;
  Mntcache *hash;
  Mntcache *prev;
  Mntcache *next;
};


struct Mount
{
    // ref<Chan>
    Chan* to;     /* channel replacing channel */

    // enum<mount>, but mainly for MCREATE, the MBEFORE|MAFTER is encoded via list
    int mflag;

    // option<string>
    char  *spec; // for mount(), nil for bind()
    ulong mountid;
    // list<ref<Mount>> sorted list by mount->mountid
    Mount*  order;

    Mount*  next;
    Mount*  copy;
};

struct Mhead
{
    // ref<Chan>
    Chan* from;     /* channel mounted upon */
    // list<ref<Mount>> list because of union mount (ordered via MBEFORE|MAFTER)
    Mount*  mount;      /* what's mounted upon it */

    Ref;
    RWlock  lock;
    // hash<qid.path, list<ref<Mhead>> of Pgrp.mnthash
    Mhead*  hash;     /* Hash chain */
};



//#include <fcall.h>

//// was in devmnt.c
//struct Mntrpc
//{
//  Chan* c;    /* Channel for whom we are working */
//  Mntrpc* list;   /* Free/pending list */
//  Fcall request;  /* Outgoing file system protocol message */
//  Fcall   reply;    /* Incoming reply */
//  Mnt*  m;    /* Mount device during rpc */
//  Rendez  r;    /* Place to hang out */
//  byte*  rpc;    /* I/O Data buffer */
//  uint  rpclen;   /* len of buffer */
//  Block *b;   /* reply blocks */
//  char  done;   /* Rpc completed */
//  uvlong  stime;    /* start time for mnt statistics */
//  ulong reqlen;   /* request length for mnt statistics */
//  ulong replen;   /* reply length for mnt statistics */
//  Mntrpc* flushed;  /* message this one flushes */
//};
//
//struct Mnt
//{
//  Lock;
//  /* references are counted using c->ref; channels on this mount point incref(c->mchan) == Mnt.c */
//  Chan  *c;   /* Channel to file service */
//  Proc  *rip;   /* Reader in progress */
//  Mntrpc  *queue;   /* Queue of pending requests on this channel */
//  ulong id;   /* Multiplexer id for channel check */
//  Mnt *list;    /* Free list */
//  int flags;    /* cache */
//  int msize;    /* data + IOHDRSZ */
//  char  *version; /* 9P version */
//  Queue *q;   /* input queue */
//};



struct Path
{
    char  *s;
    int len;      /* strlen(s) */
    int alen;     /* allocated length of s */

    // array<option<ref_counted<Chan>>, nil for elements which are not mount point
    Chan  **mtpt;     /* mtpt history */
    int mlen;     /* number of path elements */
    int malen;      /* allocated length of mtpt */
 
    // extra
    Ref;
};

/*
 * Access types in namec
 */
enum AccessFlags
{
  Aopen,        /* for i/o */
  Acreate,      /* is to be created */
  Aremove,      /* will be removed by caller */
  Aaccess,      /* as in stat, wstat */
  Atodir,       /* as in chdir */
  Abind,        /* for left-hand-side of bind */
  Amount,       /* to be mounted or mounted upon */
};

/*
 * channel flags
 */
enum ChannelFlag
{
  COPEN = 0x0001,   /* for i/o */
  CFREE = 0x0010,   /* not in use */
  CCEXEC  = 0x0008,   /* close on exec */
  CRCLOSE = 0x0020,   /* remove on close */
  CMSG  = 0x0002,   /* the message channel for a mount */
  CCACHE  = 0x0080,   /* client cache */
};

struct Chan
{
    ushort type; // idx in devtab
    ulong dev;
    Qid qid;

    Path* path;

    vlong offset;     /* in fd */

    //enum<open>, actually restricted to the OREAD|OWRITE|ORDWR of open type
    ushort mode;     /* read/write */

    bool ismtpt; // is a mount point

    union {
       void* aux; // generic pointer, for specific usages
       /*Pipe*/void* chanpipe; // for pipes
       Qid pgrpid;   /* for #p/notepg */
       ulong mid;    /* for ns in devproc */
    };
    // enum<channelflag>> (actually a bitset for certain properties)
    ushort  flag;
    Mhead*  umh;      /* mount point that derived Chan; used in unionread */
    vlong devoffset;    /* in underlying device; see read */
    Chan* umc;      /* channel in union; held for union read */
    QLock umqlock;    /* serialize unionreads */
    int uri;      /* union read index */
    Chan* mchan;      /* channel to mounted server */
    byte*  dirrock;    /* directory entry rock for translations */
    int nrock;
    int mrock;
    QLock rockqlock;
    int dri;      /* devdirread index */
    int fid;      /* for devmnt */
    ulong iounit;     /* chunk size for i/o; 0==default */

    Mnt*  mux;      /* Mnt for clients using me for messages */

    Qid mqid;     /* qid of root of mount point */
    Mntcache* mcp;      /* Mount cache pointer */

    // extra
    Ref; /* the Lock in this Ref is also Chan's lock */
    // list<ref<Chan> of Chanalloc.free
    Chan* next;     /* allocation */
    // list<ref<Chan> of Chanalloc.list
    Chan* link;
};


struct Evalue
{
    // string
    char  *name;
    // option<array<byte>, usually a string but can be something else
    byte  *value;
    // nelem(Evalue.value), 
    int len;
    
    Qid qid;
};

struct Egrp
{
    // array<ref_counted<Evalue>>
    Evalue  **ent;
    // used entries, <= ment
    int nent;
    // nelem(Egrp.ent), malloc'ed entries
    int ment; 
    
    ulong path; /* qid.path of next Evalue to be allocated */
    ulong vers; /* of Egrp */
    
    // extra
    Ref;
    RWlock;
};


// internals

struct Walkqid
{
  Chan  *clone;
  int nqid;
  // variable array length, size = nqid
  Qid qid[1];
};

struct Dev
{
    Rune dc; // dev character code, e.g. '/' (devroot), 'e' (devenv), ...
    char* name;

    void  (*reset)(void); // done once at boot time
    void  (*init)(void);
    void  (*close)(Chan*);
    Chan* (*open)(Chan*, int);
    long  (*read)(Chan*, void*, long, vlong);
    long  (*write)(Chan*, void*, long, vlong);
    Walkqid*(*walk)(Chan*, Chan*, char**, int);
    void  (*create)(Chan*, char*, int, ulong);
    void  (*remove)(Chan*);
    int (*stat)(Chan*, byte*, int);
    int (*wstat)(Chan*, byte*, int);
    Chan* (*attach)(char*);
    Block* (*bread)(Chan*, long, ulong);
    long  (*bwrite)(Chan*, Block*, ulong);
    void  (*shutdown)(void);
};


// array<Dev>, it looks like an allocated array<ref<dev>> but
// it is really a static array put here to avoid backward deps on conf_devtab,
// and it is not really a <ref<dev>> because it's pointers to static
// structures (e.g. mousedevtab, vgadevtab, etc).
extern Dev** devtab;

struct Dirtab
{
  char  name[KNAMELEN];
  Qid qid;
  vlong length;
  long  perm;
};

// used by with devmnt.c and sysfile.c
struct Bogus {
    Chan    *chan;
    Chan    *authchan;
    char    *spec;
    int flags;
};


//*****************************************************************************
// Internal to files/
//*****************************************************************************

enum
{
    TAGSHIFT = 5,           /* ulong has to be 32 bits */
    TAGMASK = (1<<TAGSHIFT)-1,
    NMASK = (64*1024)>>TAGSHIFT,
};

// actually internal to devmnt.c and mnt.c
/*
 * References are managed as follows:
 * The channel to the server - a network connection or pipe - has one
 * reference for every Chan open on the server.  The server channel has
 * c->mux set to the Mnt used for muxing control to that server.  Mnts
 * have no reference count; they go away when c goes away.
 * Each channel derived from the mount point has mchan set to c,
 * and increfs/decrefs mchan to manage references on the server
 * connection.
 */
struct Mntalloc
{
    Mnt*    list;       /* Mount devices in use */
    Mnt*    mntfree;    /* Free list */
    Mntrpc* rpcfree;
    int nrpcfree;
    int nrpcused;
    ulong   id;
    ulong   tagmask[NMASK];

    // extra
    Lock;

};
extern struct Mntalloc mntalloc;
