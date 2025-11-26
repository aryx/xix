struct	Case
{
	Case*	link;
	vlong	val;
	long	label;
	char	def;
	char isv;
};

struct	C1
{
	vlong	val;
	long	label;
};

struct	Multab
{
	long	val;
	char	code[20];
};

struct	Hintab
{
	ushort	val;
	char	hint[10];
};

struct	Var
{
	long	offset;
	Sym*	sym;
	char	name;
	char	etype;
};

struct	Reg
{
	long	pc;
	long	rpo;		/* reverse post ordering */

	Bits	set;
	Bits	use1;
	Bits	use2;

	Bits	refbehind;
	Bits	refahead;
	Bits	calbehind;
	Bits	calahead;
	Bits	regdiff;
	Bits	act;

	long	regu;
	long	loop;		/* could be shorter */

	Reg*	log5;
	long	active;

	Reg*	p1;
	Reg*	p2;
	Reg*	p2link;
	Reg*	s1;
	Reg*	s2;
	Reg*	link;
	Prog*	prog;
};
#define	R	((Reg*)0)

#define	NRGN	600
struct	Rgn
{
	Reg*	enter;
	short	cost;
	short	varno;
	short	regno;
};

//goken: new
EXTERN int retok;

EXTERN	Case*	cases;

EXTERN	Node	fconstnode;
EXTERN	Node	vconstnode;


EXTERN	long	cursafe;


EXTERN	Multab	multab[20];
EXTERN	int	hintabsize;
EXTERN	Node*	nodrat;

EXTERN	Node*	nodsafe;
EXTERN	long	nrathole;



EXTERN	Node	vregnode;
EXTERN	char	string[NSNAME];
EXTERN	Sym*	symrathole;



EXTERN	long	exregoffset;
EXTERN	long	exfregoffset;

#define	BLOAD(r)	band(bnot(r->refbehind), r->refahead)
#define	BSTORE(r)	band(bnot(r->calbehind), r->calahead)
#define	LOAD(r)		(~r->refbehind.b[z] & r->refahead.b[z])
#define	STORE(r)	(~r->calbehind.b[z] & r->calahead.b[z])

#define	bset(a,n)	((a).b[(n)/32]&(1L<<(n)%32))

#define	CLOAD	4
#define	CREF	5
#define	CINF	1000
#define	LOOP	3

EXTERN	Rgn	region[NRGN];
EXTERN	Rgn*	rgp;
EXTERN	int	nregion;
EXTERN	int	nvar;

EXTERN	Bits	externs;
EXTERN	Bits	params;
EXTERN	Bits	consts;
EXTERN	Bits	addrs;

EXTERN	long	regbits;
EXTERN	long	exregbits;

EXTERN	int	change;
EXTERN	int	suppress;

EXTERN	Reg*	firstr;
EXTERN	Reg*	lastr;
EXTERN	Reg	zreg;
EXTERN	Reg*	freer;
EXTERN	Var	var[NVAR];
EXTERN	long*	idom;
EXTERN	Reg**	rpo2r;
EXTERN	long	maxnr;
