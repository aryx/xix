#pragma	src	"/sys/src/libip"
#pragma	lib	"libip.a"

enum 
{
    IPaddrlen=	16,
    IPv4addrlen=	4,
    IPv4off=	12,

    IPllen=		4,
    IPV4HDR_LEN=	20,

    /* vihl & vcf[0] values */
    IP_VER4= 	0x40,
    IP_VER6=	0x60,
};

typedef uchar ipv4[IPv4addrlen];
typedef uchar ipaddr[IPaddrlen];
typedef ulong iplong;


// forward decl
typedef struct Ipifc Ipifc;
typedef struct Iplifc Iplifc;
typedef struct Ipv6rp Ipv6rp;
typedef struct Ip6hdr Ip6hdr;
typedef struct Icmp6hdr Icmp6hdr;
typedef struct Udphdr Udphdr;

/*
 *  for reading /net/ipifc
 */

/* local address */
struct Iplifc
{
    /* per address on the ip interface */
    ipaddr	ip;
    ipaddr	mask;
    ipaddr	net;		/* ip & mask */

    ulong	preflt;			/* preferred lifetime */
    ulong	validlt;		/* valid lifetime */

    // Extra
    // list<ref_own<Iplifc>>, head = Ipifc.lifc
    Iplifc	*next;
};

/* default values, one per stack */
struct Ipv6rp
{
    int	mflag;
    int	oflag;
    int maxraint;
    int	minraint;
    int	linkmtu;
    int	reachtime;
    int	rxmitra;
    int	ttl;
    int	routerlt;	
};

/* actual interface */
struct Ipifc
{
    /* per ip interface */

    char 	dev[64]; // e.g. "/net/ether0"
    int		mtu;
    // list<ref_own<Iplifc> (next = Iplifc.next)
    Iplifc	*lifc;

    ulong	pktin;
    ulong	pktout;
    ulong	errin;
    ulong	errout;
    Ipv6rp	rp;
    uchar	sendra6;		/* on == send router adv */
    uchar	recvra6;		/* on == rcv router adv */

    //Extra
    Ipifc	*next;
    int	index;			/* number of interface in ipifc dir */
};

#define ISIPV6MCAST(addr)	((addr)[0] == 0xff)
#define ISIPV6LINKLOCAL(addr) ((addr)[0] == 0xfe && ((addr)[1] & 0xc0) == 0x80)

/*
 * ipv6 constants
 * `ra' is `router advertisement', `rs' is `router solicitation'.
 * `na' is `neighbour advertisement'.
 */
enum {
    IPV6HDR_LEN	= 40,

    /* neighbour discovery option types */
    V6nd_srclladdr	= 1,
    V6nd_targlladdr	= 2,
    V6nd_pfxinfo	= 3,
    V6nd_redirhdr	= 4,
    V6nd_mtu	= 5,
    /* new since rfc2461; see iana.org/assignments/icmpv6-parameters */
    V6nd_home	= 8,
    V6nd_srcaddrs	= 9,		/* rfc3122 */
    V6nd_ip		= 17,
    /* /lib/rfc/drafts/draft-jeong-dnsop-ipv6-dns-discovery-12.txt */
    V6nd_rdns	= 25,
    /* plan 9 extensions */
    V6nd_9fs	= 250,
    V6nd_9auth	= 251,

    /* Router constants (all times in ms.) */
    Maxv6initraintvl= 16000,
    Maxv6initras	= 3,
    Maxv6finalras	= 3,
    Minv6interradelay= 3000,
    Maxv6radelay	= 500,

    /* Host constants */
    Maxv6rsdelay	= 1000,
    V6rsintvl	= 4000,
    Maxv6rss	= 3,

    /* Node constants */
    Maxv6mcastrss	= 3,
    Maxv6unicastrss	= 3,
    Maxv6anycastdelay= 1000,
    Maxv6na		= 3,
    V6reachabletime	= 30000,
    V6retranstimer	= 1000,
    V6initprobedelay= 5000,
};


/* V6 header on the wire */
struct Ip6hdr {
    uchar	vcf[4];		/* version:4, traffic class:8, flow label:20 */
    uchar	ploadlen[2];	/* payload length: packet length - 40 */
    uchar	proto;		/* next header type */
    uchar	ttl;		/* hop limit */
    ipaddr	src;	/* source address */
    ipaddr	dst;	/* destination address */
    uchar	payload[];
};

/*
 *  user-level icmpv6 with control message "headers"
 */
struct Icmp6hdr {
    uchar	_0_[8];
    ipaddr	laddr;	/* local address */
    ipaddr	raddr;	/* remote address */
};

/*
 *  user level udp headers with control message "headers"
 */
#define Udphdrsize 52 /* size of a Udphdr */

struct Udphdr
{
    ipaddr	raddr;	/* V6 remote address */
    ipaddr	laddr;	/* V6 local address */
    ipaddr	ifcaddr;	/* V6 ifc addr msg was received on */

    uchar	rport[2];		/* remote port */
    uchar	lport[2];		/* local port */
};

uchar*	defmask(ipaddr);
void	maskip(ipaddr, ipaddr, ipaddr);
int		eipfmt(Fmt*);
bool	isv4(ipaddr);
vlong	parseip(uchar*, char*);
vlong	parseipmask(uchar*, char*);
char*	v4parseip(uchar*, char*);
//char*	v4parsecidr(uchar*, uchar*, char*);
int	parseether(uchar*, char*);
int	myipaddr(uchar*, char*);
int	myetheraddr(uchar*, char*);
int	equivip4(uchar*, uchar*);
int	equivip6(uchar*, uchar*);

Ipifc*	readipifc(char*, Ipifc*, int);

void	hnputs(void*, ushort);
void	hnputl(void*, uint);
void	hnputv(void*, uvlong);
ushort	nhgets(void*);
uint	nhgetl(void*);
uvlong	nhgetv(void*);

ushort	ptclbsum(uchar*, int);

int		v6tov4(uchar*, uchar*);
void	v4tov6(uchar*, uchar*);

#define	ipcmp(x, y) memcmp(x, y, IPaddrlen)
#define	ipmove(x, y) memmove(x, y, IPaddrlen)

extern ipaddr IPv4bcast;
extern ipaddr IPv4bcastobs;
extern ipaddr IPv4allsys;
extern ipaddr IPv4allrouter;
extern ipaddr IPnoaddr;
extern ipaddr v4prefix;
extern ipaddr IPallbits;

#define CLASS(p) ((*(uchar*)(p))>>6)

#pragma	varargck	type	"I"	uchar*
#pragma	varargck	type	"V"	uchar*
#pragma	varargck	type	"E"	uchar*
#pragma	varargck	type	"M"	uchar*
